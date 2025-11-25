module Physics.Collision where

import Types
import Constants
import Systems.DamageSystem
import qualified Systems.TrapSystem as TrapSystem
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Physics.Projectiles (calculateVelocityToTarget)

-- ============================================================================
-- Collision Detection
-- ============================================================================

handleProjectileCollisions :: World -> World
handleProjectileCollisions world =
  let projectileList = M.elems (projectiles world)
      (world', _) = foldl checkProjectileCollision (world, []) projectileList
  in world'

checkProjectileCollision :: (World, [EntityId]) -> Projectile -> (World, [EntityId])
checkProjectileCollision (world, hitList) projectile =
  case projectileTargetId projectile of
    Nothing -> (world, hitList)
    Just targetId ->
      case M.lookup targetId (enemies world) of
        Nothing ->
          let projectiles' = M.delete (projectileId projectile) (projectiles world)
          in (world { projectiles = projectiles' }, hitList)
        Just enemy ->
          -- Update projectile velocity to track target on first check
          let projectile' = if projectileVel projectile == (0, 0)
                            then projectile { projectileVel = calculateVelocityToTarget (projectilePos projectile) (enemyPos enemy) }
                            else projectile
          in if distance (projectilePos projectile') (enemyPos enemy) < 20
             then
               let enemy' = applyDamageToEnemy (projectileDamage projectile') enemy
                   enemies' = M.insert targetId enemy' (enemies world)
                   projectiles' = if projectilePiercing projectile'
                                  then projectiles world
                                  else M.delete (projectileId projectile') (projectiles world)
                   effect = ImpactFlash (enemyPos enemy) 0.2 0.2
                   effects' = effect : visualEffects world
               in (world { enemies = enemies', projectiles = projectiles', visualEffects = effects' }, targetId : hitList)
             else
               -- Update projectile position and velocity
               let projectiles' = M.insert (projectileId projectile') projectile' (projectiles world)
               in (world { projectiles = projectiles' }, hitList)

handleEnemyFortCollisions :: World -> World
handleEnemyFortCollisions world =
  let enemyList = M.elems (enemies world)
      world' = foldl checkEnemyFortCollision world enemyList
  in world'

checkEnemyFortCollision :: World -> Enemy -> World
checkEnemyFortCollision world enemy =
  case enemyAIState enemy of
    AttackingGate ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        let gate' = applyDamageToGate (enemyDamage enemy) (fortGate $ fort world)
            fort' = (fort world) { fortGate = gate' }
            gatePos' = gatePos gate'
            effect = GateFlash 0.15
            -- Add attack particle from enemy to gate
            attackParticle = EnemyAttackParticle (enemyPos enemy) gatePos' 0.2
            effects' = effect : attackParticle : visualEffects world
            enemy' = enemy { enemyLastAttackTime = timeElapsed world }
            enemies' = M.insert (enemyId enemy) enemy' (enemies world)
        in world { fort = fort', enemies = enemies', visualEffects = effects' }
      else world
    
    AttackingTower tid ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        case M.lookup tid (towers world) of
          Nothing -> world
          Just tower ->
            let tower' = applyDamageToTower (enemyDamage enemy) tower
                impactEffect = ImpactFlash (towerPos tower) 0.15 0.15
                -- Add attack particle from enemy to tower
                attackParticle = EnemyAttackParticle (enemyPos enemy) (towerPos tower) 0.2
                effects' = impactEffect : attackParticle : visualEffects world
                enemy' = enemy { enemyLastAttackTime = timeElapsed world }
                enemies' = M.insert (enemyId enemy) enemy' (enemies world)
                -- Remove tower if destroyed
                towers' = if towerHP tower' <= 0
                         then M.delete tid (towers world)
                         else M.insert tid tower' (towers world)
            in world { towers = towers', enemies = enemies', visualEffects = effects' }
      else world
    
    AttackingCastle ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        let castle' = applyDamageToCastle (enemyDamage enemy) (castle world)
            castlePos' = castlePos $ castle world
            effect = CastleFlash 0.15
            -- Add attack particle from enemy to castle
            attackParticle = EnemyAttackParticle (enemyPos enemy) castlePos' 0.2
            effects' = effect : attackParticle : visualEffects world
            enemy' = enemy { enemyLastAttackTime = timeElapsed world }
            enemies' = M.insert (enemyId enemy) enemy' (enemies world)
        in world { castle = castle', enemies = enemies', visualEffects = effects' }
      else world
    
    _ -> world

handleEnemyTrapCollisions :: World -> World
handleEnemyTrapCollisions world =
  let enemyList = M.elems (enemies world)
      trapList = M.elems (traps world)
      (world', _) = foldl (\acc enemy -> checkEnemyTraps acc enemy trapList) (world, []) enemyList
  in world'

checkEnemyTraps :: (World, [EntityId]) -> Enemy -> [Trap] -> (World, [EntityId])
checkEnemyTraps (world, affected) enemy traps =
  let nearbyTraps = filter (\t -> distance (trapPos t) (enemyPos enemy) < 30) traps
      (world', affected') = foldl (\acc trap -> triggerTrapOnEnemy acc enemy trap) (world, affected) nearbyTraps
  in (world', affected')

triggerTrapOnEnemy :: (World, [EntityId]) -> Enemy -> Trap -> (World, [EntityId])
triggerTrapOnEnemy (world, affected) enemy trap =
  if S.member (enemyId enemy) (trapAffectedEnemies trap) && trapType trap /= SpikeTrap && trapType trap /= FirePitTrap && trapType trap /= ExplosiveBarrel
  then (world, affected)
  else
    let (trap', effects) = TrapSystem.triggerTrap (enemyId enemy) trap enemy
        damage = trapDamage (trapType trap)
        enemy' = TrapSystem.applyTrapEffects trap' $ applyDamageToEnemy damage enemy
        
        traps' = M.insert (trapId trap) trap' (traps world)
        enemies' = M.insert (enemyId enemy) enemy' (enemies world)
        effects' = effects ++ visualEffects world
        
        traps'' = if TrapSystem.shouldRemoveTrap trap'
                  then M.delete (trapId trap) traps'
                  else traps'
    in (world { traps = traps'', enemies = enemies', visualEffects = effects' }, enemyId enemy : affected)

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)