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
  -- If the gate is destroyed, ensure enemies progress inside the fort.
  let s = enemyAIState enemy
      isAlreadyInsideOrCombat st =
        case st of
          InsideFort -> True
          AttackingTower _ -> True
          AttackingCastle -> True
          Dead -> True
          _ -> False
      gatePos' = gatePos $ fortGate $ fort world
      distToGate = Path.distance (enemyPos enemy) gatePos'
  in if gateDestroyed (fortGate $ fort world) && not (isAlreadyInsideOrCombat s)
     then 
       -- Gate is destroyed, transition to InsideFort state (movement will handle progression)
       enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
     else
       case s of
         MovingToFort -> updateMovingToFort dt world enemy
         AttackingGate -> updateAttackingGate dt world enemy
         AttackingWall wid -> updateAttackingWall dt world wid enemy
         ClimbingWall wid -> updateClimbingWall dt world wid enemy
         InsideFort -> updateInsideFort dt world enemy
         AttackingTower tid -> updateAttackingTower dt world tid enemy
         AttackingCastle -> updateAttackingCastle dt world enemy
         Dead -> enemy

-- ============================================================================
-- Moving to Fort
-- ============================================================================

updateMovingToFort :: Float -> World -> Enemy -> Enemy
updateMovingToFort dt world enemy =
  let gateLocation = gatePos $ fortGate $ fort world
      distToGate = Path.distance (enemyPos enemy) gateLocation
  in if distToGate < 200  -- Large threshold to ensure enemies attack gate
     then
       if gateDestroyed (fortGate $ fort world)
       then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
       else enemy { enemyAIState = AttackingGate }
     else
       -- Keep moving toward fort if still far away
       case Path.getNextPathPoint enemy world of
         Just waypoint ->
           if Path.hasReachedWaypoint enemy waypoint
           then Path.advancePathIndex enemy
           else
             if enemyCanClimb enemy && distToGate < 250
             then case Path.findNearestClimbPoint enemy world of
                    Just (wid, _) -> enemy { enemyAIState = ClimbingWall wid }
                    Nothing -> enemy
             else enemy
         Nothing ->
           -- Ran out of waypoints but still not at gate - try to attack anyway
           if gateDestroyed (fortGate $ fort world)
           then enemy { enemyAIState = InsideFort, enemyPathIndex = 0 }
           else enemy { enemyAIState = AttackingGate }

-- ============================================================================
-- Attacking Gate
-- ============================================================================

updateAttackingGate :: Float -> World -> Enemy -> Enemy
updateAttackingGate dt world enemy =
  if gateDestroyed (fortGate $ fort world)
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