module Physics.PhysicsCore where

import Types
import qualified AI.Pathfinding as Path
import qualified Data.Map.Strict as M

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
      let dir = Path.directionTo (enemyPos enemy) waypoint
          spd = enemySpeed enemy * enemySlowFactor enemy
          vel = (fst dir * spd, snd dir * spd)
          newPos = (fst (enemyPos enemy) + fst vel * dt,
                    snd (enemyPos enemy) + snd vel * dt)
      in enemy { enemyPos = newPos, enemyVel = vel }