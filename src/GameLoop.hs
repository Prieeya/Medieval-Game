module GameLoop where

import Types
import Constants
import Config
import qualified Physics.PhysicsCore as Physics
import qualified Physics.Collision as Collision
import qualified Physics.Projectiles as Projectiles
import qualified AI.FSM as FSM
import qualified AI.ThreatAnalysis as ThreatAnalysis
import qualified AI.Director as Director
import qualified Systems.WaveSystem as WaveSystem
import qualified Systems.TowerSystem as TowerSystem
import qualified Systems.TrapSystem as TrapSystem
import qualified Systems.DamageSystem as DamageSystem
import qualified Systems.AbilitySystem as AbilitySystem
import qualified Systems.CastleSystem as CastleSystem
import qualified Systems.FortSystem as FortSystem
import qualified Data.Map.Strict as M

-- ============================================================================
-- Main Update Function
-- ============================================================================

updateWorld :: Float -> World -> World
updateWorld dt world
  | isPaused world = world
  | isGameOver world = world
  | otherwise = 
      let dt' = dt * gameSpeedValue (gameSpeed world)
          world1 = world { timeElapsed = timeElapsed world + dt' }
          world2 = updateGameSystems dt' world1
          world3 = checkVictoryDefeat world2
      in world3

updateGameSystems :: Float -> World -> World
updateGameSystems dt world =
  let world1 = WaveSystem.updateWaveSystem dt world
      world2 = updateAbilities dt world1
      world3 = updateEnemies dt world2
      world4 = updateTowers dt world3
      world5 = updateTraps dt world4
      world6 = updateProjectiles dt world5
      world7 = handleCollisions world6
      world8 = applyDamage world7
      -- Re-update enemy AI states after damage is applied and gate status updated
      world8' = let enemies' = M.map (updateEnemy dt world8) (enemies world8)
                in world8 { enemies = enemies' }
      world9 = cleanupDead world8'
      world10 = updateVisualEffects dt world9
      world11 = updateThreatAnalysis world10
  in world11

-- ============================================================================
-- Enemy Updates
-- ============================================================================

updateEnemies :: Float -> World -> World
updateEnemies dt world =
  let enemies' = M.map (updateEnemy dt world) (enemies world)
  in world { enemies = enemies' }

updateEnemy :: Float -> World -> Enemy -> Enemy
updateEnemy dt world enemy =
  let enemy1 = FSM.updateEnemyAI dt world enemy
      enemy2 = Physics.moveEnemy dt world enemy1
      enemy3 = updateEnemyEffects dt enemy2
  in enemy3

updateEnemyEffects :: Float -> Enemy -> Enemy
updateEnemyEffects dt enemy =
  let burnDur = max 0 (enemyBurnDuration enemy - dt)
      hitFlash = max 0 (enemyHitFlash enemy - dt)
      enemy1 = enemy { enemyBurnDuration = burnDur, enemyHitFlash = hitFlash }
      enemy2 = if burnDur > 0
               then enemy1 { enemyHP = enemyHP enemy1 - burnDamagePerSecond * dt }
               else enemy1
  in enemy2

-- ============================================================================
-- Tower Updates
-- ============================================================================

updateTowers :: Float -> World -> World
updateTowers dt world =
  let towers' = M.map (updateTower dt world) (towers world)
      (towers'', newProjectiles) = TowerSystem.fireTowers dt world towers'
      allProjectiles = M.union newProjectiles (projectiles world)
  in world { towers = towers'', projectiles = allProjectiles }

updateTower :: Float -> World -> Tower -> Tower
updateTower dt world tower =
  TowerSystem.acquireTarget world tower

-- ============================================================================
-- Trap Updates
-- ============================================================================

updateTraps :: Float -> World -> World
updateTraps dt world =
  TrapSystem.updateTraps dt world

-- ============================================================================
-- Projectile Updates
-- ============================================================================

updateProjectiles :: Float -> World -> World
updateProjectiles dt world =
  let projectiles' = M.map (Projectiles.updateProjectile dt) (projectiles world)
      projectiles'' = M.filter (\p -> projectileLifetime p > 0) projectiles'
  in world { projectiles = projectiles'' }

-- ============================================================================
-- Collision Detection & Handling
-- ============================================================================

handleCollisions :: World -> World
handleCollisions world =
  let world1 = Collision.handleProjectileCollisions world
      world2 = Collision.handleEnemyFortCollisions world1
      world3 = Collision.handleEnemyTrapCollisions world2
  in world3

-- ============================================================================
-- Damage Application
-- ============================================================================

applyDamage :: World -> World
applyDamage world = DamageSystem.applyQueuedDamage world

-- ============================================================================
-- Cleanup Dead Entities
-- ============================================================================

cleanupDead :: World -> World
cleanupDead world =
  let (enemies', gold) = cleanupDeadEnemies (enemies world)
      projectiles' = M.filter (\p -> projectileLifetime p > 0) (projectiles world)
      resources' = (resources world) { resGold = resGold (resources world) + gold }
  in world { enemies = enemies', projectiles = projectiles', resources = resources' }

cleanupDeadEnemies :: M.Map EntityId Enemy -> (M.Map EntityId Enemy, Int)
cleanupDeadEnemies enemiesMap =
  let (alive, dead) = M.partition (\e -> enemyHP e > 0) enemiesMap
      goldEarned = sum $ map (enemyGoldValue . enemyType) $ M.elems dead
  in (alive, goldEarned)

-- ============================================================================
-- Visual Effects
-- ============================================================================

updateVisualEffects :: Float -> World -> World
updateVisualEffects dt world =
  let effects' = map (updateEffect dt) (visualEffects world)
      effects'' = filter isEffectAlive effects'
  in world { visualEffects = effects'' }

updateEffect :: Float -> VisualEffect -> VisualEffect
updateEffect dt (ImpactFlash pos life maxLife) =
  ImpactFlash pos (life - dt) maxLife
updateEffect dt (ExplosionEffect pos life maxLife) =
  ExplosionEffect pos (life - dt) maxLife
updateEffect dt (FireBurst pos life) =
  FireBurst pos (life - dt)
updateEffect dt (TarSplash pos life) =
  TarSplash pos (life - dt)
updateEffect dt (SpikePopup pos life) =
  SpikePopup pos (life - dt)
updateEffect dt (GateFlash life) =
  GateFlash (life - dt)
updateEffect dt (CastleFlash life) =
  CastleFlash (life - dt)
updateEffect dt (DamageNumber pos dmg life) =
  DamageNumber pos dmg (life - dt)
updateEffect dt (EnemyAttackParticle from to life) =
  EnemyAttackParticle from to (life - dt)

isEffectAlive :: VisualEffect -> Bool
isEffectAlive (ImpactFlash _ life _) = life > 0
isEffectAlive (ExplosionEffect _ life _) = life > 0
isEffectAlive (FireBurst _ life) = life > 0
isEffectAlive (TarSplash _ life) = life > 0
isEffectAlive (SpikePopup _ life) = life > 0
isEffectAlive (GateFlash life) = life > 0
isEffectAlive (CastleFlash life) = life > 0
isEffectAlive (DamageNumber _ _ life) = life > 0
isEffectAlive (EnemyAttackParticle _ _ life) = life > 0

-- ============================================================================
-- Ability Updates
-- ============================================================================

updateAbilities :: Float -> World -> World
updateAbilities dt world =
  AbilitySystem.updateAbilities dt world

-- ============================================================================
-- Threat Analysis
-- ============================================================================

updateThreatAnalysis :: World -> World
updateThreatAnalysis world =
  let threatData' = ThreatAnalysis.analyzeThreat world
  in world { threatData = threatData' }

-- ============================================================================
-- Victory/Defeat Conditions
-- ============================================================================

checkVictoryDefeat :: World -> World
checkVictoryDefeat world
  | castleHP (castle world) <= 0 = world { isGameOver = True, isVictory = False }
  | wsLevel (waveState world) > 5 && wsLevelCleared (waveState world) =
      world { isGameOver = True, isVictory = True }
  | otherwise = world