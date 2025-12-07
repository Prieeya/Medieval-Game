module Physics.PhysicsCore where

import Types
import qualified AI.Pathfinding as Path
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- Enemy Movement
-- ============================================================================

moveEnemy :: Float -> World -> Enemy -> Enemy
moveEnemy dt world enemy =
  case enemyAIState enemy of
    MovingToFort -> moveAlongPath dt world enemy
    InsideFort -> moveAlongPath dt world enemy
    AttackingTower tid ->
      -- Move toward tower if not in range
      case M.lookup tid (towers world) of
        Nothing -> enemy  -- Tower destroyed, will transition in FSM
        Just tower ->
          let dist = Path.distance (enemyPos enemy) (towerPos tower)
          in if dist > enemyAttackRange enemy
             then
               -- Move toward tower
               let dir = Path.directionTo (enemyPos enemy) (towerPos tower)
                   spd = enemySpeed enemy * enemySlowFactor enemy
                   vel = (fst dir * spd, snd dir * spd)
                   newPos = (fst (enemyPos enemy) + fst vel * dt,
                             snd (enemyPos enemy) + snd vel * dt)
               in enemy { enemyPos = newPos, enemyVel = vel }
             else enemy  -- In range, stop moving
    AttackingCastle ->
      -- Move toward castle if not in range
      let castlePos' = castlePos $ castle world
          dist = Path.distance (enemyPos enemy) castlePos'
      in if dist > enemyAttackRange enemy
         then
           -- Move toward castle
           let dir = Path.directionTo (enemyPos enemy) castlePos'
               spd = enemySpeed enemy * enemySlowFactor enemy
               vel = (fst dir * spd, snd dir * spd)
               newPos = (fst (enemyPos enemy) + fst vel * dt,
                         snd (enemyPos enemy) + snd vel * dt)
           in enemy { enemyPos = newPos, enemyVel = vel }
         else enemy  -- In range, stop moving
    ClimbingWall _ -> enemy
    _ -> enemy

moveAlongPath :: Float -> World -> Enemy -> Enemy
moveAlongPath dt world enemy =
  case Path.getNextPathPoint enemy world of
    Nothing -> enemy
    Just waypoint ->
      let baseDir = Path.directionTo (enemyPos enemy) waypoint
          
          -- Smart AI: Avoid known traps
          shouldAvoid = enemyType enemy `notElem` [GruntRaider, Direwolf]
          avoidance = if shouldAvoid 
                      then calculateAvoidance (enemyPos enemy) (knownTraps world)
                      else (0, 0)
          
          -- Combine base direction with avoidance vector
          -- Avoidance has high weight to ensure they go around
          combinedX = fst baseDir + fst avoidance * 3.0
          combinedY = snd baseDir + snd avoidance * 3.0
          
          finalDir = normalize (combinedX, combinedY)
          
          spd = enemySpeed enemy * enemySlowFactor enemy
          vel = (fst finalDir * spd, snd finalDir * spd)
          newPos = (fst (enemyPos enemy) + fst vel * dt,
                    snd (enemyPos enemy) + snd vel * dt)
      in enemy { enemyPos = newPos, enemyVel = vel }

-- Calculate repulsion vector from known traps
calculateAvoidance :: Vec2 -> S.Set Vec2 -> Vec2
calculateAvoidance pos traps =
  let nearbyTraps = filter (\t -> Path.distance pos t < 150) (S.toList traps)
      forces = map (\t -> 
        let d = Path.distance pos t
            dir = Path.directionTo t pos -- Repel FROM trap
            -- Strength decreases with distance
            strength = max 0 (1.0 - d / 150)
        in (fst dir * strength, snd dir * strength)) nearbyTraps
      totalForce = foldl (\(accX, accY) (fx, fy) -> (accX + fx, accY + fy)) (0, 0) forces
  in totalForce

normalize :: Vec2 -> Vec2
normalize (x, y) =
  let len = sqrt (x*x + y*y)
  in if len > 0.01 then (x/len, y/len) else (0, 0)