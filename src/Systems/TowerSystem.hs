module Systems.TowerSystem where

import Types
import Constants
import Config
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L

-- ============================================================================
-- Tower Targeting
-- ============================================================================

acquireTarget :: World -> Tower -> Tower
acquireTarget world tower =
  let validTargets = findValidTargets world tower
      prioritizedTargets = prioritizeTargets tower validTargets world
  in case prioritizedTargets of
    [] -> tower { towerTargetId = Nothing }
    (target:_) -> tower { towerTargetId = Just (enemyId target) }

findValidTargets :: World -> Tower -> [Enemy]
findValidTargets world tower =
  let enemyList = M.elems (enemies world)
      inRange = filter (\e -> distance (towerPos tower) (enemyPos e) <= towerRange tower) enemyList
      alive = filter (\e -> enemyHP e > 0) inRange
  in alive

prioritizeTargets :: Tower -> [Enemy] -> World -> [Enemy]
prioritizeTargets tower targets world =
  case towerType tower of
    ArrowTower -> prioritizeArcherTargets targets
    _ -> prioritizeDefaultTargets targets world

-- CRITICAL: Archer towers prioritize climbers first
prioritizeArcherTargets :: [Enemy] -> [Enemy]
prioritizeArcherTargets targets =
  let climbing = filter (\e -> case enemyAIState e of
                                ClimbingWall _ -> True
                                _ -> False) targets
      insideFort = filter (\e -> case enemyAIState e of
                                  InsideFort -> True
                                  AttackingTower _ -> True
                                  AttackingCastle -> True
                                  _ -> False) targets
      attackingGate = filter (\e -> case enemyAIState e of
                                     AttackingGate -> True
                                     _ -> False) targets
      others = filter (\e -> case enemyAIState e of
                              MovingToFort -> True
                              _ -> False) targets
  in climbing ++ insideFort ++ attackingGate ++ others

prioritizeDefaultTargets :: [Enemy] -> World -> [Enemy]
prioritizeDefaultTargets targets world =
  let castle' = castle world
      byDistance = L.sortBy (\e1 e2 -> compare
                              (distance (enemyPos e1) (castlePos castle'))
                              (distance (enemyPos e2) (castlePos castle'))) targets
  in byDistance

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- ============================================================================
-- Tower Firing
-- ============================================================================

fireTowers :: Float -> World -> M.Map EntityId Tower -> (M.Map EntityId Tower, M.Map EntityId Projectile)
fireTowers dt world towers =
  let towerList = M.elems towers
      (towers', projectiles) = foldr (fireTower dt world) (towers, M.empty) towerList
  in (towers', projectiles)

fireTower :: Float -> World -> Tower -> (M.Map EntityId Tower, M.Map EntityId Projectile) -> (M.Map EntityId Tower, M.Map EntityId Projectile)
fireTower dt world tower (towerMap, projectileMap) =
  let canFire = (timeElapsed world - towerLastFireTime tower) >= towerFireRate tower
      hasTarget = case towerTargetId tower of
                    Just tid -> M.member tid (enemies world)
                    Nothing -> False
  in if canFire && hasTarget
     then
       let Just targetId = towerTargetId tower
           projectile = createProjectileFrom tower targetId (timeElapsed world)
           tower' = tower { towerLastFireTime = timeElapsed world }
           towerMap' = M.insert (towerId tower) tower' towerMap
           projectileMap' = M.insert (projectileId projectile) projectile projectileMap
       in (towerMap', projectileMap')
     else (towerMap, projectileMap)

createProjectileFrom :: Tower -> EntityId -> Float -> Projectile
createProjectileFrom tower targetId time =
  let pType = towerTypeToProjectileType (towerType tower)
      (piercing, aoe) = case towerType tower of
                          BallistaTower -> (True, 0)  -- Piercing
                          CatapultTower -> (False, 60)  -- AoE splash
                          FireTower -> (False, 50)  -- AoE DoT
                          TeslaTower -> (False, lightningChainRange)  -- Chain lightning
                          BombardTower -> (False, 80)  -- AoE burst
                          _ -> (False, 0)
  in Projectile
    { projectileId = towerId tower * 10000 + round time
    , projectileType = pType
    , projectilePos = towerPos tower
    , projectileVel = (0, 0)
    , projectileDamage = towerDamage tower
    , projectileTargetId = Just targetId
    , projectileSourceId = towerId tower
    , projectileLifetime = 10.0
    , projectilePiercing = piercing
    , projectileAoERadius = aoe
    , projectileHitEnemies = S.empty
    }

towerTypeToProjectileType :: TowerType -> ProjectileType
towerTypeToProjectileType ArrowTower = Arrow
towerTypeToProjectileType CatapultTower = CatapultRock
towerTypeToProjectileType CrossbowTower = BallistaBolt
towerTypeToProjectileType FireTower = Fireball
towerTypeToProjectileType TeslaTower = LightningBolt
towerTypeToProjectileType BallistaTower = BallistaBolt
towerTypeToProjectileType PoisonTower = Arrow  -- Could be a poison arrow
towerTypeToProjectileType BombardTower = CatapultRock