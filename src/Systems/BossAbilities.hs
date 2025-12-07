-- Boss Abilities System
module Systems.BossAbilities where

import Types
import Constants
import Config
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- Boss Ability Cooldowns
-- ============================================================================

bossAbilityCooldown :: UnitType -> Float
bossAbilityCooldown IronbackMinotaur = 8.0  -- Charge every 8 seconds
bossAbilityCooldown FireDrake = 6.0         -- Fire breath every 6 seconds
bossAbilityCooldown LichKingArcthros = 10.0 -- Summon every 10 seconds
bossAbilityCooldown _ = 999.0  -- Non-bosses have very long cooldown

bossSpawnCooldown :: UnitType -> Float
bossSpawnCooldown IronbackMinotaur = 15.0   -- Spawn grunts every 15 seconds
bossSpawnCooldown FireDrake = 12.0          -- Spawn pyromancers every 12 seconds
bossSpawnCooldown LichKingArcthros = 8.0    -- Spawn skeletons every 8 seconds
bossSpawnCooldown _ = 999.0

-- ============================================================================
-- Update Boss Abilities
-- ============================================================================

updateBossAbilities :: Float -> World -> Enemy -> (Enemy, World)
updateBossAbilities dt world enemy
  | enemyRole enemy /= Boss = (enemy, world)
  | enemyHP enemy <= 0 = (enemy, world)
  | otherwise = 
      let currentTime = timeElapsed world
          abilityCD = bossAbilityCooldown (enemyType enemy)
          spawnCD = bossSpawnCooldown (enemyType enemy)
          
          -- Update timers
          timeSinceLastAbility = currentTime - bossLastAbilityTime enemy
          timeSinceLastSpawn = currentTime - bossSpawnTimer enemy
          
          -- Try to use abilities
          (enemy1, world1) = if timeSinceLastAbility >= abilityCD
                            then useBossAbility enemy world
                            else (enemy, world)
          
          -- Try to spawn minions
          (enemy2, world2) = if timeSinceLastSpawn >= spawnCD
                            then spawnBossMinions enemy1 world1
                            else (enemy1, world1)
      in (enemy2, world2)

-- ============================================================================
-- Boss Special Abilities
-- ============================================================================

useBossAbility :: Enemy -> World -> (Enemy, World)
useBossAbility enemy world = case enemyType enemy of
  IronbackMinotaur -> useChargeAttack enemy world
  FireDrake -> useFireBreath enemy world
  LichKingArcthros -> useDeathAura enemy world
  _ -> (enemy, world)

-- Ironback Minotaur: Charge attack (dash forward)
useChargeAttack :: Enemy -> World -> (Enemy, World)
useChargeAttack enemy world =
  let (x, y) = enemyPos enemy
      gatePos' = gatePos $ fortGate $ fort world
      (gx, gy) = gatePos'
      -- Charge direction toward gate
      dx = gx - x
      dy = gy - y
      dist = sqrt (dx*dx + dy*dy)
      (dirX, dirY) = if dist > 0 then (dx/dist, dy/dist) else (1, 0)
      -- Charge speed boost (3x normal speed for 1 second)
      chargeSpeed = enemySpeed enemy * 3.0
      newVel = (dirX * chargeSpeed, dirY * chargeSpeed)
      enemy' = enemy 
        { enemyVel = newVel
        , bossLastAbilityTime = timeElapsed world
        , bossAbilityCooldown = bossAbilityCooldown (enemyType enemy)
        }
  in (enemy', world)

-- Fire Drake: Fire breath AoE attack
useFireBreath :: Enemy -> World -> (Enemy, World)
useFireBreath enemy world =
  let (x, y) = enemyPos enemy
      gatePos' = gatePos $ fortGate $ fort world
      (gx, gy) = gatePos'
      (dx, dy) = (gx - x, gy - y)
      dist = sqrt (dx*dx + dy*dy)
      (dirX, dirY) = if dist > 0 then (dx/dist, dy/dist) else (1, 0)
      allTowersMap = towers world
      
      -- Find towers in cone in front of drake
      affectedTowers = M.filter checkTower allTowersMap
        where
          checkTower tower =
            let (tx, ty) = towerPos tower
                (tdx, tdy) = (tx - x, ty - y)
                tdist = sqrt (tdx*tdx + tdy*tdy)
                dot = if tdist > 0.1 then (tdx*dirX + tdy*dirY) / tdist else 0.0
            in tdist < 200 && dot > 0.5
      
      -- Apply fire damage to towers
      towers' = M.map (\tower -> 
        let damage = 50.0
            newHP = max 0 (towerHP tower - damage)
        in tower { towerHP = newHP }
      ) affectedTowers
      
      -- Merge damaged towers back
      allTowers = M.union towers' allTowersMap
      enemy' = enemy
        { bossLastAbilityTime = timeElapsed world
        , bossAbilityCooldown = bossAbilityCooldown (enemyType enemy)
        }
  in (enemy', world { towers = allTowers })

-- Lich King: Death aura (damage nearby towers)
useDeathAura :: Enemy -> World -> (Enemy, World)
useDeathAura enemy world =
  let (x, y) = enemyPos enemy
      -- Damage all towers within 150 range
      affectedTowers = M.filter (\tower ->
        let (tx, ty) = towerPos tower
            (tdx, tdy) = (tx - x, ty - y)
            tdist = sqrt (tdx*tdx + tdy*tdy)
        in tdist < 150
      ) (towers world)
      
      -- Apply death damage (magic damage, ignores some armor)
      towers' = M.map (\tower -> 
        let damage = 30.0  -- Death aura damage
            newHP = max 0 (towerHP tower - damage)
        in tower { towerHP = newHP }
      ) affectedTowers
      
      allTowers = M.union towers' (towers world)
      enemy' = enemy
        { bossLastAbilityTime = timeElapsed world
        , bossAbilityCooldown = bossAbilityCooldown (enemyType enemy)
        }
  in (enemy', world { towers = allTowers })

-- ============================================================================
-- Boss Minion Spawning
-- ============================================================================

spawnBossMinions :: Enemy -> World -> (Enemy, World)
spawnBossMinions enemy world = case enemyType enemy of
  IronbackMinotaur -> spawnGrunts enemy world
  FireDrake -> spawnPyromancers enemy world
  LichKingArcthros -> spawnSkeletons enemy world
  _ -> (enemy, world)

spawnGrunts :: Enemy -> World -> (Enemy, World)
spawnGrunts enemy world =
  let (x, y) = enemyPos enemy
      spawnCount = 2  -- Spawn 2 grunts
      newEnemies = foldl (\acc i ->
        let angle = fromIntegral i * 2 * pi / fromIntegral spawnCount
            offsetX = cos angle * 40
            offsetY = sin angle * 40
            spawnPos = (x + offsetX, y + offsetY)
            newEnemy = createEnemy (nextEntityId world + i) GruntRaider spawnPos (enemySpawnSide enemy) (timeElapsed world)
        in M.insert (enemyId newEnemy) newEnemy acc
      ) (enemies world) [0..spawnCount-1]
      enemy' = enemy { bossSpawnTimer = timeElapsed world }
  in (enemy', world { enemies = newEnemies, nextEntityId = nextEntityId world + spawnCount })

spawnPyromancers :: Enemy -> World -> (Enemy, World)
spawnPyromancers enemy world =
  let (x, y) = enemyPos enemy
      spawnCount = 2  -- Spawn 2 pyromancers
      newEnemies = foldl (\acc i ->
        let angle = fromIntegral i * 2 * pi / fromIntegral spawnCount
            offsetX = cos angle * 50
            offsetY = sin angle * 50
            spawnPos = (x + offsetX, y + offsetY)
            newEnemy = createEnemy (nextEntityId world + i) Pyromancer spawnPos (enemySpawnSide enemy) (timeElapsed world)
        in M.insert (enemyId newEnemy) newEnemy acc
      ) (enemies world) [0..spawnCount-1]
      enemy' = enemy { bossSpawnTimer = timeElapsed world }
  in (enemy', world { enemies = newEnemies, nextEntityId = nextEntityId world + spawnCount })

spawnSkeletons :: Enemy -> World -> (Enemy, World)
spawnSkeletons enemy world =
  let (x, y) = enemyPos enemy
      spawnCount = 3  -- Spawn 3 skeletons (skeleton minions)
      newEnemies = foldl (\acc i ->
        let angle = fromIntegral i * 2 * pi / fromIntegral spawnCount
            offsetX = cos angle * 45
            offsetY = sin angle * 45
            spawnPos = (x + offsetX, y + offsetY)
            -- Use GruntRaider as skeleton proxy (or create SkeletonMinion type)
            newEnemy = createEnemy (nextEntityId world + i) GruntRaider spawnPos (enemySpawnSide enemy) (timeElapsed world)
        in M.insert (enemyId newEnemy) newEnemy acc
      ) (enemies world) [0..spawnCount-1]
      enemy' = enemy { bossSpawnTimer = timeElapsed world }
  in (enemy', world { enemies = newEnemies, nextEntityId = nextEntityId world + spawnCount })

