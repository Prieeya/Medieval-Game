module AI.FSM where

import Types
import Constants
import qualified AI.Pathfinding as Path
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- ============================================================================
-- Enemy AI State Machine
-- ============================================================================

updateEnemyAI :: Float -> World -> Enemy -> Enemy
updateEnemyAI dt world enemy =
  -- If any gate the enemy targets is destroyed, ensure enemies progress inside the fort.
  let s = enemyAIState enemy
      isAlreadyInsideOrCombat st =
        case st of
          InsideFort -> True
          AttackingTower _ -> True
          AttackingCastle -> True
          AttackingTrap _ -> True
          Dead -> True
          _ -> False
      gates = fortGates (fort world)
      targetGateIdx = enemyTargetGate enemy
      targetGate = if targetGateIdx < length gates 
                   then gates !! targetGateIdx 
                   else head gates
  in if gateDestroyed targetGate && not (isAlreadyInsideOrCombat s)
     then 
       -- Gate is destroyed, transition to InsideFort state (movement will handle progression)
       enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
     else
       -- Check for revealed traps nearby (enemies attack revealed traps)
       case findNearbyRevealedTrap enemy world of
         Just trapId | shouldAttackTrap enemy -> 
           enemy { enemyAIState = AttackingTrap trapId }
         _ ->
           case s of
             MovingToFort -> updateMovingToFort dt world enemy
             AttackingGate gateIdx -> updateAttackingGate dt world gateIdx enemy
             AttackingWall wid -> updateAttackingWall dt world wid enemy
             ClimbingWall wid -> updateClimbingWall dt world wid enemy
             InsideFort -> updateInsideFort dt world enemy
             AttackingTower tid -> updateAttackingTower dt world tid enemy
             AttackingTrap tid -> updateAttackingTrap dt world tid enemy
             AttackingCastle -> updateAttackingCastle dt world enemy
             Dead -> enemy

-- Check if enemy should attack traps (TrapBreaker always does, others sometimes)
shouldAttackTrap :: Enemy -> Bool
shouldAttackTrap enemy = 
  enemyType enemy == TrapBreaker || 
  (PreferTraps `elem` enemyTargetPrefs enemy)

-- Find a revealed trap nearby that the enemy can attack
-- Traps are revealed when enemies see them (handled in collision system)
findNearbyRevealedTrap :: Enemy -> World -> Maybe EntityId
findNearbyRevealedTrap enemy world =
  let trapList = M.elems (traps world)
      -- Attack range: enemies can attack revealed traps within 60 pixels
      attackRange = 60.0
      revealedTraps = filter trapRevealed trapList
      nearbyTraps = filter (\t -> Path.distance (enemyPos enemy) (trapPos t) < attackRange) revealedTraps
  in case nearbyTraps of
       [] -> Nothing
       (t:_) -> Just (trapId t)

-- Update enemy attacking a trap
updateAttackingTrap :: Float -> World -> EntityId -> Enemy -> Enemy
updateAttackingTrap dt world tid enemy =
  case M.lookup tid (traps world) of
    Nothing -> enemy { enemyAIState = MovingToFort }  -- Trap destroyed
    Just trap ->
      let dist = Path.distance (enemyPos enemy) (trapPos trap)
      in if dist > enemyAttackRange enemy + 20
         then enemy { enemyAIState = MovingToFort }  -- Move closer
         else enemy  -- In range, stay attacking

-- ============================================================================
-- Moving to Fort
-- ============================================================================

updateMovingToFort :: Float -> World -> Enemy -> Enemy
updateMovingToFort dt world enemy =
  let targetGateIdx = enemyTargetGate enemy
      gates = fortGates (fort world)
      targetGate = if targetGateIdx < length gates 
                   then gates !! targetGateIdx 
                   else head gates  -- Fallback to first gate
      gateLocation = gatePos targetGate
      -- Add attack offset for random positioning around gate
      (offX, offY) = enemyAttackOffset enemy
      attackPos = (fst gateLocation + offX, snd gateLocation + offY)
      distToGate = Path.distance (enemyPos enemy) attackPos
      prefersWalls = PreferWalls `elem` enemyTargetPrefs enemy
      canClimb = enemyCanClimb enemy
  in 
     -- Wall climbing enemies prioritize climbing walls over attacking gate
     if canClimb && prefersWalls && distToGate < 350
     then case Path.findNearestClimbPoint enemy world of
            Just (wid, climbPos) ->
              let distToClimb = Path.distance (enemyPos enemy) climbPos
              in if distToClimb < 30
                 then enemy { enemyAIState = ClimbingWall wid, enemyLastAttackTime = timeElapsed world }
                 else enemy  -- Keep moving toward climb point
            Nothing ->
              -- No climb points, attack wall directly if close
              case findNearestWall enemy world of
                Just (wid, wallDist) | wallDist < 50 -> enemy { enemyAIState = AttackingWall wid }
                _ -> if distToGate < 100
                     then if gateDestroyed targetGate
                          then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
                          else enemy { enemyAIState = AttackingGate targetGateIdx }
                     else enemy
     else if distToGate < 100  -- Threshold to start attacking
     then
       if gateDestroyed targetGate
       then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
       else enemy { enemyAIState = AttackingGate targetGateIdx }
     else
       -- Keep moving toward fort if still far away
       case Path.getNextPathPoint enemy world of
         Just waypoint ->
           if Path.hasReachedWaypoint enemy waypoint
           then Path.advancePathIndex enemy
           else enemy
         Nothing ->
           -- Ran out of waypoints but still not at gate - try to attack anyway
           if gateDestroyed targetGate
           then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
           else enemy { enemyAIState = AttackingGate targetGateIdx }

-- Find nearest wall segment for attacking
findNearestWall :: Enemy -> World -> Maybe (Int, Float)
findNearestWall enemy world =
  let walls = fortWalls (fort world)
      wallsWithDist = map (\w -> 
        let midX = (fst (wallStart w) + fst (wallEnd w)) / 2
            midY = (snd (wallStart w) + snd (wallEnd w)) / 2
            dist = Path.distance (enemyPos enemy) (midX, midY)
        in (Types.wallId w, dist)) walls
      validWalls = filter (\(_, d) -> d < 500) wallsWithDist
      sorted = L.sortBy (\(_, d1) (_, d2) -> compare d1 d2) validWalls
  in case sorted of
       [] -> Nothing
       ((wid, d):_) -> Just (wid, d)

-- ============================================================================
-- Attacking Gate
-- ============================================================================

updateAttackingGate :: Float -> World -> Int -> Enemy -> Enemy
updateAttackingGate dt world gateIdx enemy =
  let gates = fortGates (fort world)
      targetGate = if gateIdx < length gates 
                   then gates !! gateIdx 
                   else head gates
  in if gateDestroyed targetGate
     then 
       -- Gate is destroyed, transition to InsideFort (movement will handle progression)
       enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
     else enemy

-- ============================================================================
-- Attacking Wall
-- ============================================================================

updateAttackingWall :: Float -> World -> Int -> Enemy -> Enemy
updateAttackingWall dt world wallId enemy =
  let walls = fortWalls (fort world)
      wall = filter (\w -> wallId == Types.wallId w) walls
  in case wall of
    [] -> enemy { enemyAIState = MovingToFort }
    (w:_) ->
      if wallHP w <= 0
      then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
      else enemy

-- ============================================================================
-- Climbing Wall
-- ============================================================================

updateClimbingWall :: Float -> World -> Int -> Enemy -> Enemy
updateClimbingWall dt world wallId enemy =
  let walls = fortWalls (fort world)
      wall = filter (\w -> wallId == Types.wallId w) walls
  in case wall of
    [] -> enemy { enemyAIState = MovingToFort }
    (w:_) ->
      -- Simple timer-based climbing
      if enemyLastAttackTime enemy + climbDuration < timeElapsed world
      then enemy
        { enemyAIState = InsideFort
        , enemyPathIndex = 0
        , enemyPos = case wallClimbPoint w of
                      Just cp -> cp
                      Nothing -> enemyPos enemy
        }
      else enemy

-- ============================================================================
-- Inside Fort
-- ============================================================================

updateInsideFort :: Float -> World -> Enemy -> Enemy
updateInsideFort dt world enemy =
  -- Prioritize attacking towers and interior defences over moving to castle
  let nearbyTowers = findNearbyTowers enemy world
      castlePos' = castlePos $ castle world
      distToCastle = Path.distance (enemyPos enemy) castlePos'
  in case nearbyTowers of
    [] ->
      -- No nearby towers, continue to castle
      if distToCastle < enemyAttackRange enemy
      then enemy { enemyAIState = AttackingCastle }
      else
        case Path.getNextPathPoint enemy world of
          Nothing -> enemy { enemyAIState = AttackingCastle }
          Just waypoint ->
            if Path.hasReachedWaypoint enemy waypoint
            then Path.advancePathIndex enemy
            else enemy
    (tid:_) -> 
      -- Found a tower, transition to AttackingTower state
      -- Movement toward tower will be handled by PhysicsCore
      enemy { enemyAIState = AttackingTower tid }

findNearbyTowers :: Enemy -> World -> [EntityId]
findNearbyTowers enemy world =
  let towerList = M.elems (towers world)
      -- Use larger detection range to find nearby towers (attack range + 200 for early detection)
      nearby = filter (\t -> Path.distance (enemyPos enemy) (towerPos t) < enemyAttackRange enemy + 200) towerList
      sorted = L.sortBy (\t1 t2 -> compare (Path.distance (enemyPos enemy) (towerPos t1)) (Path.distance (enemyPos enemy) (towerPos t2))) nearby
  in map towerId sorted

-- ============================================================================
-- Attacking Tower
-- ============================================================================

updateAttackingTower :: Float -> World -> EntityId -> Enemy -> Enemy
updateAttackingTower dt world tid enemy =
  case M.lookup tid (towers world) of
    Nothing -> enemy { enemyAIState = InsideFort }
    Just tower ->
      let dist = Path.distance (enemyPos enemy) (towerPos tower)
      in if dist > enemyAttackRange enemy + 50  -- Add buffer for movement
         then enemy { enemyAIState = InsideFort }  -- Move closer via InsideFort state
         else enemy  -- In range, stay in AttackingTower state to attack

-- ============================================================================
-- Attacking Castle
-- ============================================================================

updateAttackingCastle :: Float -> World -> Enemy -> Enemy
updateAttackingCastle dt world enemy =
  let castlePos' = castlePos $ castle world
      distToCastle = Path.distance (enemyPos enemy) castlePos'
  in if castleHP (castle world) <= 0
     then enemy { enemyAIState = Dead }
     else if distToCastle > enemyAttackRange enemy + 50
          then enemy { enemyAIState = InsideFort }  -- Move closer if too far
          else enemy