module Systems.AbilitySystem where

import Types
import Constants
import Systems.DamageSystem
import qualified Data.Map.Strict as M

-- ============================================================================
-- Ability System
-- ============================================================================

updateAbilities :: Float -> World -> World
updateAbilities dt world =
  let abilities' = M.map (updateAbility dt) (abilities world)
      world' = world { abilities = abilities' }
      world'' = applyActiveAbilities dt world'
  in world''

updateAbility :: Float -> AbilityState -> AbilityState
updateAbility dt ability =
  let cooldown' = max 0 (abilityCooldown ability - dt)
      duration' = max 0 (abilityDuration ability - dt)
      active' = duration' > 0
  in ability
    { abilityCooldown = cooldown'
    , abilityDuration = duration'
    , abilityActive = active'
    }

activateAbility :: AbilityType -> World -> World
activateAbility abilityType world =
  case M.lookup abilityType (abilities world) of
    Nothing -> world
    Just ability ->
      if abilityCooldown ability <= 0
      then
        let ability' = ability
              { abilityCooldown = abilityCooldowns abilityType
              , abilityDuration = abilityDurations abilityType
              , abilityActive = True
              }
            abilities' = M.insert abilityType ability' (abilities world)
            world' = world { abilities = abilities' }
            world'' = applyAbilityEffect abilityType world'
        in world''
      else world

applyAbilityEffect :: AbilityType -> World -> World
applyAbilityEffect RepairWalls world =
  let fort' = repairFort (fort world)
  in world { fort = fort' }
applyAbilityEffect _ world = world

repairFort :: Fort -> Fort
repairFort fort =
  let gate' = (fortGate fort) { gateHP = Constants.gateMaxHP, gateDestroyed = False }
      walls' = map (\w -> w { wallHP = Constants.wallMaxHP }) (fortWalls fort)
  in fort { fortGate = gate', fortWalls = walls' }

applyActiveAbilities :: Float -> World -> World
applyActiveAbilities dt world =
  let abilities' = abilities world
      world1 = if isAbilityActive Firestorm abilities'
               then applyFirestorm dt world
               else world
      world2 = if isAbilityActive FreezeField abilities'
               then applyFreezeField dt world1
               else world1
      world3 = if isAbilityActive TimeSlow abilities'
               then world2
               else world2
  in world3

isAbilityActive :: AbilityType -> M.Map AbilityType AbilityState -> Bool
isAbilityActive abilityType abilities =
  case M.lookup abilityType abilities of
    Just ability -> abilityActive ability
    Nothing -> False

applyFirestorm :: Float -> World -> World
applyFirestorm dt world =
  let enemies' = M.map (\e -> applyDamageToEnemy (50 * dt) e) (enemies world)
  in world { enemies = enemies' }

applyFreezeField :: Float -> World -> World
applyFreezeField dt world =
  let enemies' = M.map (\e -> e { enemySlowFactor = 0.3 }) (enemies world)
  in world { enemies = enemies' }