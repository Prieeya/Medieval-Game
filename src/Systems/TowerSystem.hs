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
      
      -- Special Rule: Only Archer-type towers can target enemies climbing walls
      targetable = filter (\e -> case enemyAIState e of
                                   ClimbingWall _ -> towerType tower `elem` [ArrowTower, CrossbowTower, PoisonTower]
                                   _ -> True) alive
  in targetable

prioritizeTargets :: Tower -> [Enemy] -> World -> [Enemy]
prioritizeTargets tower targets world =
  case towerType tower of
    -- Archer-type towers prioritize climbers and inside-fort enemies
    ArrowTower -> prioritizeArcherTargets targets
    CrossbowTower -> prioritizeArcherTargets targets
    PoisonTower -> prioritizeArcherTargets targets
    -- Siege towers also prioritize inside-fort threats
    CatapultTower -> prioritizeSiegeTargets targets world
    BombardTower -> prioritizeSiegeTargets targets world
    _ -> prioritizeDefaultTargets targets world

-- CRITICAL: Archer towers prioritize climbers and inside-fort enemies first
prioritizeArcherTargets :: [Enemy] -> [Enemy]
prioritizeArcherTargets targets =
  let -- Highest priority: enemies climbing walls
      climbing = filter (\e -> case enemyAIState e of
                               ClimbingWall _ -> True
                               _ -> False) targets
      -- Second priority: enemies already inside fort attacking defenses
      insideFort = filter (\e -> case enemyAIState e of
                                 InsideFort -> True
                                 AttackingTower _ -> True
                                 AttackingCastle -> True
                                 _ -> False) targets
      -- Third priority: enemies attacking walls (trying to breach)
      attackingWall = filter (\e -> case enemyAIState e of
                                    AttackingWall _ -> True
                                    _ -> False) targets
      -- Fourth priority: enemies attacking gate
      attackingGate = filter (\e -> case enemyAIState e of
                                    AttackingGate -> True
                                    _ -> False) targets
      -- Lowest priority: enemies still moving
      others = filter (\e -> case enemyAIState e of
                             MovingToFort -> True
                             _ -> False) targets
  in climbing ++ insideFort ++ attackingWall ++ attackingGate ++ others

-- Siege towers prioritize enemies inside fort and high-threat targets
prioritizeSiegeTargets :: [Enemy] -> World -> [Enemy]
prioritizeSiegeTargets targets world =
  let -- Highest priority: enemies inside fort attacking castle/towers
      insideFort = filter (\e -> case enemyAIState e of
                                 InsideFort -> True
                                 AttackingTower _ -> True
                                 AttackingCastle -> True
                                 _ -> False) targets
      insideFortIds = map enemyId insideFort
      -- Second: climbers
      climbing = filter (\e -> case enemyAIState e of
                               ClimbingWall _ -> True
                               _ -> False) targets
      climbingIds = map enemyId climbing
      -- Third: heavy/siege units
      heavyUnits = filter (\e -> enemyRole e `elem` [Heavy, Siege, Boss]) targets
      heavyIds = map enemyId heavyUnits
      -- Rest by distance (exclude already prioritized)
      castle' = castle world
      allPriorityIds = insideFortIds ++ climbingIds ++ heavyIds
      others = L.sortBy (\e1 e2 -> compare
                          (distance (enemyPos e1) (castlePos castle'))
                          (distance (enemyPos e2) (castlePos castle'))) 
               (filter (\e -> enemyId e `notElem` allPriorityIds) targets)
  in insideFort ++ climbing ++ heavyUnits ++ others

prioritizeDefaultTargets :: [Enemy] -> World -> [Enemy]
prioritizeDefaultTargets targets world =
  let -- Still prioritize inside-fort threats
      insideFort = filter (\e -> case enemyAIState e of
                                 InsideFort -> True
                                 AttackingTower _ -> True
                                 AttackingCastle -> True
                                 ClimbingWall _ -> True
                                 _ -> False) targets
      insideFortIds = map enemyId insideFort
      castle' = castle world
      others = L.sortBy (\e1 e2 -> compare
                          (distance (enemyPos e1) (castlePos castle'))
                          (distance (enemyPos e2) (castlePos castle'))) 
               (filter (\e -> enemyId e `notElem` insideFortIds) targets)
  in insideFort ++ others

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- ============================================================================
-- Tower Firing
-- ============================================================================

fireTowers :: Float -> World -> M.Map EntityId Tower -> (M.Map EntityId Tower, M.Map EntityId Projectile, [SoundEvent])
fireTowers dt world towers =
  let towerList = M.elems towers
      (towers', projectiles, events) = foldr (fireTower dt world) (towers, M.empty, []) towerList
  in (towers', projectiles, events)

fireTower :: Float -> World -> Tower -> (M.Map EntityId Tower, M.Map EntityId Projectile, [SoundEvent]) -> (M.Map EntityId Tower, M.Map EntityId Projectile, [SoundEvent])
fireTower dt world tower (towerMap, projectileMap, events) =
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
           events' = SoundTowerFire (towerType tower) : events
       in (towerMap', projectileMap', events')
     else (towerMap, projectileMap, events)

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
      initialAnim = AnimationState { animType = AnimFlying, animFrame = 0, animTime = 0 }
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
    , projectileAnimState = initialAnim
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