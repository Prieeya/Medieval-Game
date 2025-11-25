module Systems.TrapSystem where

import Types
import Constants
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- Trap System
-- ============================================================================

updateTraps :: Float -> World -> World
updateTraps dt world =
  let traps' = M.map (updateTrap dt) (traps world)
  in world { traps = traps' }

updateTrap :: Float -> Trap -> Trap
updateTrap dt trap =
  trap { trapActiveTime = trapActiveTime trap + dt }

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
      enemy { enemySlowFactor = trapSlowFactor FreezeTrap }
    MagicSnareTrap ->
      enemy { enemySlowFactor = 0.0 }  -- Root (0 speed)
    FirePitTrap ->
      enemy  -- Continuous damage handled elsewhere
    _ -> enemy

shouldRemoveTrap :: Trap -> Bool
shouldRemoveTrap trap =
  case trapType trap of
    SpikeTrap -> trapTriggered trap
    ExplosiveBarrel -> trapTriggered trap
    _ -> False