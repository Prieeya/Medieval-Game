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
                   -- Queue hit sounds
                   soundHit = SoundProjectileHit (projectileType projectile')
                   -- Only play enemy hit sound sometimes to avoid spam, or always? Always for now.
                   soundEnemy = SoundEnemyHit (enemyType enemy)
                   events' = soundHit : soundEnemy : soundEvents world
               in (world { enemies = enemies', projectiles = projectiles', visualEffects = effects', soundEvents = events' }, targetId : hitList)
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
    AttackingGate gateIdx ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        let gates = fortGates (fort world)
            targetGate = if gateIdx < length gates then gates !! gateIdx else head gates
            gate' = applyDamageToGate (enemyDamage enemy) targetGate
            -- Update the specific gate in the list
            gates' = take gateIdx gates ++ [gate'] ++ drop (gateIdx + 1) gates
            fort' = (fort world) { fortGates = gates' }
            gatePos' = gatePos gate'
            effect = GateFlash 0.15
            -- Add attack particle from enemy to gate
            attackParticle = EnemyAttackParticle (enemyPos enemy) gatePos' 0.2
            effects' = effect : attackParticle : visualEffects world
            enemy' = enemy { enemyLastAttackTime = timeElapsed world }
            enemies' = M.insert (enemyId enemy) enemy' (enemies world)
            -- Sounds
            soundAttack = SoundEnemyAttack (enemyType enemy)
            soundGate = if gateDestroyed gate' && not (gateDestroyed targetGate) 
                        then SoundGateDestroyed 
                        else SoundGateHit
            events' = soundAttack : soundGate : soundEvents world
        in world { fort = fort', enemies = enemies', visualEffects = effects', soundEvents = events' }
      else world
    
    AttackingWall wid ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        let walls = fortWalls (fort world)
            (walls', effects, events) = attackWall wid (enemyDamage enemy) (enemyPos enemy) walls world
            fort' = (fort world) { fortWalls = walls' }
            enemy' = enemy { enemyLastAttackTime = timeElapsed world }
            enemies' = M.insert (enemyId enemy) enemy' (enemies world)
            effects' = effects ++ visualEffects world
            events' = events ++ soundEvents world
        in world { fort = fort', enemies = enemies', visualEffects = effects', soundEvents = events' }
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
                -- Sounds
                soundAttack = SoundEnemyAttack (enemyType enemy)
                events = if towerHP tower' <= 0 
                         then SoundTowerDestroyed : soundAttack : soundEvents world
                         else soundAttack : soundEvents world
            in world { towers = towers', enemies = enemies', visualEffects = effects', soundEvents = events }
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
            -- Sounds
            soundAttack = SoundEnemyAttack (enemyType enemy)
            soundCastle = SoundCastleHit
            events' = soundAttack : soundCastle : soundEvents world
        in world { castle = castle', enemies = enemies', visualEffects = effects', soundEvents = events' }
      else world
    
    AttackingTrap tid ->
      if timeElapsed world - enemyLastAttackTime enemy >= enemyAttackCooldown enemy
      then
        case M.lookup tid (traps world) of
          Nothing -> world  -- Trap already destroyed
          Just trap ->
            let -- Apply damage to trap
                newHP = trapHP trap - enemyDamage enemy
                trap' = trap { trapHP = newHP }
                impactEffect = ImpactFlash (trapPos trap) 0.15 0.15
                attackParticle = EnemyAttackParticle (enemyPos enemy) (trapPos trap) 0.2
                effects' = impactEffect : attackParticle : visualEffects world
                enemy' = enemy { enemyLastAttackTime = timeElapsed world }
                enemies' = M.insert (enemyId enemy) enemy' (enemies world)
                -- Remove trap if destroyed
                traps' = if newHP <= 0
                         then M.delete tid (traps world)
                         else M.insert tid trap' (traps world)
                -- Sound
                soundAttack = SoundEnemyAttack (enemyType enemy)
                events = soundAttack : soundEvents world
            in world { traps = traps', enemies = enemies', visualEffects = effects', soundEvents = events }
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
  let nearbyTraps = filter (\t -> distance (trapPos t) (enemyPos enemy) < 25) traps
      (world', affected') = foldl (\acc trap -> triggerTrapOnEnemy acc enemy trap) (world, affected) nearbyTraps
  in (world', affected')

triggerTrapOnEnemy :: (World, [EntityId]) -> Enemy -> Trap -> (World, [EntityId])
triggerTrapOnEnemy (world, affected) enemy trap =
  if S.member (enemyId enemy) (trapAffectedEnemies trap) && trapType trap /= SpikeTrap && trapType trap /= FirePitTrap && trapType trap /= ExplosiveBarrel
  then (world, affected)
  else
    let (trap', effects) = TrapSystem.triggerTrap (enemyId enemy) trap enemy
        
        -- Trap Breaker Logic: Disarms traps without taking damage
        isTrapBreaker = enemyType enemy == TrapBreaker
        damage = if isTrapBreaker then 0 else trapDamage (trapType trap)
        
        enemy' = TrapSystem.applyTrapEffects trap' $ applyDamageToEnemy damage enemy
        
        -- Trap is revealed when triggered
        trap'' = trap' { trapRevealed = True }
        
        traps' = M.insert (trapId trap) trap'' (traps world)
        enemies' = M.insert (enemyId enemy) enemy' (enemies world)
        effects' = effects ++ visualEffects world
        
        traps'' = if isTrapBreaker || TrapSystem.shouldRemoveTrap trap''
                  then M.delete (trapId trap) traps'
                  else traps'
        
        -- Sound
        soundTrap = SoundTrapTriggered
        events' = soundTrap : soundEvents world
        
        -- Smart AI: If enemy dies, mark this trap location as dangerous
        knownTraps' = if enemyHP enemy' <= 0
                      then S.insert (trapPos trap) (knownTraps world)
                      else knownTraps world
        
    in (world { traps = traps'', enemies = enemies', visualEffects = effects', soundEvents = events', knownTraps = knownTraps' }, enemyId enemy : affected)

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Attack a wall segment and return updated walls, effects, and sounds
attackWall :: Int -> Float -> Vec2 -> [WallSegment] -> World -> ([WallSegment], [VisualEffect], [SoundEvent])
attackWall wid damage enemyPos walls world =
  let updateWall w = 
        if Types.wallId w == wid
        then applyDamageToWall damage w
        else w
      walls' = map updateWall walls
      targetWall = filter (\w -> Types.wallId w == wid) walls
      (effects, events) = case targetWall of
        [] -> ([], [])
        (w:_) -> 
          let midX = (fst (wallStart w) + fst (wallEnd w)) / 2
              midY = (snd (wallStart w) + snd (wallEnd w)) / 2
              wallPos = (midX, midY)
              effect = ImpactFlash wallPos 0.15 0.15
              attackParticle = EnemyAttackParticle enemyPos wallPos 0.2
              soundWall = SoundGateHit  -- Reuse gate hit sound for walls
          in ([effect, attackParticle], [soundWall])
  in (walls', effects, events)