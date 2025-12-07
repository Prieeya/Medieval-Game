module Systems.TrapSystem where

import Types
import Constants
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- Trap System
-- ============================================================================

-- Trap effect durations (in seconds)
trapEffectDuration :: TrapType -> Float
trapEffectDuration FreezeTrap = 2.0      -- Slow for 2 seconds
trapEffectDuration MagicSnareTrap = 1.5  -- Root for 1.5 seconds (reduced from permanent)
trapEffectDuration FirePitTrap = 3.0     -- Burn for 3 seconds
trapEffectDuration _ = 0

updateTraps :: Float -> World -> World
updateTraps dt world =
  let traps' = M.map (updateTrap dt) (traps world)
      -- Also update enemies to recover from trap effects over time
      enemies' = M.map (recoverFromTrapEffects dt) (enemies world)
  in world { traps = traps', enemies = enemies' }

-- Enemies slowly recover their speed over time
recoverFromTrapEffects :: Float -> Enemy -> Enemy
recoverFromTrapEffects dt enemy =
  let currentSlow = enemySlowFactor enemy
      -- Recover 0.5 speed factor per second (full recovery in 2 seconds from 0)
      recoveryRate = 0.5 * dt
      newSlow = min 1.0 (currentSlow + recoveryRate)
  in if currentSlow < 1.0
     then enemy { enemySlowFactor = newSlow }
     else enemy

updateTrap :: Float -> Trap -> Trap
updateTrap dt trap =
  let newActiveTime = trapActiveTime trap + dt
      -- Clear affected enemies after effect duration expires
      duration = trapEffectDuration (trapType trap)
      shouldClear = duration > 0 && newActiveTime > duration
      newAffected = if shouldClear then S.empty else trapAffectedEnemies trap
      newActiveTime' = if shouldClear then 0 else newActiveTime
  in trap { trapActiveTime = newActiveTime', trapAffectedEnemies = newAffected }

triggerTrap :: EntityId -> Trap -> Enemy -> (Trap, [VisualEffect])
triggerTrap enemyId trap enemy =
  case trapType trap of
    SpikeTrap ->
      let effect = SpikePopup (trapPos trap) 0.5
          trap' = trap { trapTriggered = True }
      in (trap', [effect])
    
    FreezeTrap ->
      let effect = TarSplash (trapPos trap) 0.8  -- Using existing effect for freeze
          trap' = trap { trapAffectedEnemies = S.insert enemyId (trapAffectedEnemies trap) }
      in (trap', [effect])
    
    FirePitTrap ->
      let effect = FireBurst (trapPos trap) 1.5
          trap' = trap { trapAffectedEnemies = S.insert enemyId (trapAffectedEnemies trap) }
      in (trap', [effect])
    
    MagicSnareTrap ->
      let effect = TarSplash (trapPos trap) 0.8  -- Root effect
          trap' = trap { trapAffectedEnemies = S.insert enemyId (trapAffectedEnemies trap) }
      in (trap', [effect])
    
    ExplosiveBarrel ->
      let effect = ExplosionEffect (trapPos trap) 0.6 0.6
          trap' = trap { trapTriggered = True }
      in (trap', [effect])

applyTrapEffects :: Trap -> Enemy -> Enemy
applyTrapEffects trap enemy =
  case trapType trap of
    FreezeTrap ->
      -- Slow to 40% speed (not permanent, will recover)
      enemy { enemySlowFactor = trapSlowFactor FreezeTrap }
    MagicSnareTrap ->
      -- Root to 10% speed (not 0, so they can still inch forward)
      enemy { enemySlowFactor = 0.1 }
    FirePitTrap ->
      -- Slight slow from fire
      enemy { enemySlowFactor = max 0.7 (enemySlowFactor enemy) }
    _ -> enemy

shouldRemoveTrap :: Trap -> Bool
shouldRemoveTrap trap =
  case trapType trap of
    SpikeTrap -> trapTriggered trap
    ExplosiveBarrel -> trapTriggered trap
    _ -> False