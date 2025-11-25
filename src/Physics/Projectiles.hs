module Physics.Projectiles where

import Types
import Constants
import qualified Data.Map.Strict as M

-- ============================================================================
-- Projectile Movement
-- ============================================================================

updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt projectile =
  let newLifetime = projectileLifetime projectile - dt
      newPos = case projectileTargetId projectile of
                 Nothing -> moveProjectile dt projectile
                 Just _ -> moveTowardsTarget dt projectile
  in projectile { projectilePos = newPos, projectileLifetime = newLifetime }

moveProjectile :: Float -> Projectile -> Vec2
moveProjectile dt projectile =
  let (vx, vy) = projectileVel projectile
      (x, y) = projectilePos projectile
  in (x + vx * dt, y + vy * dt)

moveTowardsTarget :: Float -> Projectile -> Vec2
moveTowardsTarget dt projectile =
  let (x, y) = projectilePos projectile
      vel = projectileSpeed
  in case projectileVel projectile of
    (0, 0) -> (x, y)
    (vx, vy) ->
      let len = sqrt (vx * vx + vy * vy)
          nx = vx / len
          ny = vy / len
      in (x + nx * vel * dt, y + ny * vel * dt)

calculateVelocityToTarget :: Vec2 -> Vec2 -> Vec2
calculateVelocityToTarget from to =
  let dx = fst to - fst from
      dy = snd to - snd from
      len = sqrt (dx * dx + dy * dy)
  in if len > 0.01
     then (dx / len * projectileSpeed, dy / len * projectileSpeed)
     else (0, 0)