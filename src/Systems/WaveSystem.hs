module Systems.WaveSystem where

import Types
import qualified Constants
import Constants (wavesPerLevel, buildPhaseTime, bossPhaseTime, waveCountdownTime, baseWaveGold, spawnYRange, leftSpawnX, centerSpawnX, rightSpawnX, baseEnemyCount, enemyCountScaling)
import Config
import qualified AI.Director as Director
import qualified Data.Map.Strict as M
import System.Random

-- ============================================================================
-- Wave System Update
-- ============================================================================

updateWaveSystem :: Float -> World -> World
updateWaveSystem dt world =
  case wsPhase (waveState world) of
    BuildPhase timeLeft ->
      updateBuildPhase dt timeLeft world
    BossIncoming timeLeft ->
      updateBossIncoming dt timeLeft world
    WaveCountdown timeLeft ->
      updateWaveCountdown dt timeLeft world
    InWave ->
      updateInWave dt world

-- ============================================================================
-- Build Phase
-- ============================================================================

updateBuildPhase :: Float -> Float -> World -> World
updateBuildPhase dt timeLeft world
  | timeLeft - dt <= 0 =
      -- Start next wave
      startNextWave world
  | otherwise =
      let ws = waveState world
          ws' = ws { wsPhase = BuildPhase (timeLeft - dt) }
      in world { waveState = ws' }

-- ============================================================================
-- Boss Incoming Phase
-- ============================================================================

updateBossIncoming :: Float -> Float -> World -> World
updateBossIncoming dt timeLeft world
  | timeLeft - dt <= 0 =
      -- Start boss wave
      startBossWave world
  | otherwise =
      let ws = waveState world
          ws' = ws { wsPhase = BossIncoming (timeLeft - dt) }
      in world { waveState = ws' }

-- ============================================================================
-- Wave Countdown Phase
-- ============================================================================

updateWaveCountdown :: Float -> Float -> World -> World
updateWaveCountdown dt timeLeft world
  | timeLeft - dt <= 0 =
      -- Countdown finished, start spawning enemies
      let ws = waveState world
          ws' = ws { wsPhase = InWave }
      in world { waveState = ws' }
  | otherwise =
      let ws = waveState world
          ws' = ws { wsPhase = WaveCountdown (timeLeft - dt) }
      in world { waveState = ws' }

-- ============================================================================
-- In Wave Phase
-- ============================================================================

updateInWave :: Float -> World -> World
updateInWave dt world =
  let ws = waveState world
      
      -- Spawn enemies
      world1 = if wsEnemiesSpawned ws < wsEnemiesToSpawn ws
               then spawnEnemies dt world
               else world
      
      -- Check if wave is cleared
      world2 = if M.null (enemies world1) && wsEnemiesSpawned (waveState world1) >= wsEnemiesToSpawn (waveState world1)
               then onWaveCleared world1
               else world1
  in world2

-- ============================================================================
-- Starting Waves
-- ============================================================================

startNextWave :: World -> World
startNextWave world =
  let ws = waveState world
      nextWave = wsWaveInLevel ws + 1
      
      -- Check if this should be a boss wave
      isBossWave = nextWave > wavesPerLevel
  in if isBossWave
     then transitionToBossPrep world
     else startRegularWave nextWave world

startRegularWave :: Int -> World -> World
startRegularWave waveNum world =
  let ws = waveState world
      level = wsLevel ws
      
      -- Generate wave plan using Director
      gen = mkStdGen (round $ timeElapsed world * 1000)
      (plan, _) = Director.planNextWave world gen
      
      enemyCount = calculateEnemyCount level waveNum (dpDifficultyMult plan)
      
      ws' = ws
        { wsWaveInLevel = waveNum
        , wsPhase = WaveCountdown waveCountdownTime  -- Start with countdown
        , wsEnemiesSpawned = 0
        , wsEnemiesToSpawn = enemyCount
        , wsSpawnTimer = 0
        , wsWaveCleared = False
        }
  in world { waveState = ws', directorPlan = Just plan }

startBossWave :: World -> World
startBossWave world =
  let ws = waveState world
      level = wsLevel ws
      
      gen = mkStdGen (round $ timeElapsed world * 2000)
      (plan, _) = Director.planNextWave world gen
      
      enemyCount = calculateBossWaveCount level (dpDifficultyMult plan)
      
      ws' = ws
        { wsWaveInLevel = wavesPerLevel + 1
        , wsPhase = WaveCountdown waveCountdownTime  -- Start with countdown
        , wsEnemiesSpawned = 0
        , wsEnemiesToSpawn = enemyCount
        , wsSpawnTimer = 0
        , wsWaveCleared = False
        }
  in world { waveState = ws', directorPlan = Just plan }

-- ============================================================================
-- Wave Completion
-- ============================================================================

onWaveCleared :: World -> World
onWaveCleared world =
  let ws = waveState world
      isBossWave = wsWaveInLevel ws > wavesPerLevel
      waveInLevel = wsWaveInLevel ws
      level = wsLevel ws
      
      -- Grant wave completion gold
      -- Waves 1-2: same base amount
      -- Wave 3 (before boss): extra bonus to help afford towers
      baseReward = baseWaveGold + level * 120
      bossPrepBonus = if waveInLevel == wavesPerLevel then 400 else 0  -- Extra gold before boss wave
      gold = baseReward + bossPrepBonus
      resources' = (resources world) { resGold = resGold (resources world) + gold }
      
      world1 = world { resources = resources' }
  in if isBossWave
     then onLevelCleared world1
     else transitionToBuildPhase world1

transitionToBuildPhase :: World -> World
transitionToBuildPhase world =
  let ws = waveState world
      gates = fortGates (fort world)
      anyGateDestroyed = any gateDestroyed gates
      ws' = ws
        { wsPhase = BuildPhase buildPhaseTime
        , wsWaveCleared = True
        , wsGateRepairPending = anyGateDestroyed  -- Set repair pending if any gate is destroyed
        }
      -- Do NOT automatically repair gate - player must pay to repair
  in world { waveState = ws' }

-- Repair gate to full health
repairGate :: Gate -> Gate
repairGate gate =
  gate
    { gateHP = Constants.gateMaxHP
    , gateMaxHP = Constants.gateMaxHP
    , gateDestroyed = False
    }

transitionToBossPrep :: World -> World
transitionToBossPrep world =
  let ws = waveState world
      ws' = ws
        { wsPhase = BossIncoming bossPhaseTime
        , wsWaveInLevel = wavesPerLevel
        }
      -- Do NOT auto-repair gate - player must pay to repair
  in world { waveState = ws' }

onLevelCleared :: World -> World
onLevelCleared world =
  let ws = waveState world
      ws' = ws
        { wsLevel = wsLevel ws + 1
        , wsWaveInLevel = 0
        , wsPhase = BuildPhase buildPhaseTime
        , wsLevelCleared = True
        , wsWaveCleared = True
        }
      -- Do NOT auto-repair gate - player must pay to repair
  in world { waveState = ws', fort = fort world }

-- ============================================================================
-- Enemy Spawning
-- ============================================================================

spawnEnemies :: Float -> World -> World
spawnEnemies dt world =
  let ws = waveState world
      timer = wsSpawnTimer ws + dt
      spawnInterval = 1.0
  in if timer >= spawnInterval && wsEnemiesSpawned ws < wsEnemiesToSpawn ws
     then
       let world1 = spawnNextEnemy world
           ws' = (waveState world1)
             { wsEnemiesSpawned = wsEnemiesSpawned ws + 1
             , wsSpawnTimer = 0
             }
       in world1 { waveState = ws' }
     else
       let ws' = ws { wsSpawnTimer = timer }
       in world { waveState = ws' }

spawnNextEnemy :: World -> World
spawnNextEnemy world =
  case directorPlan world of
    Nothing -> world
    Just plan ->
      let gen = mkStdGen (round $ timeElapsed world * 3000 + fromIntegral (nextEntityId world))
          (enemyType, gen1) = selectEnemyType plan gen
          (spawnSide, gen2) = selectSpawnSide plan gen1
          (spawnY, _) = randomR spawnYRange gen2
          
          spawnX = case spawnSide of
                     LeftSide -> leftSpawnX
                     CenterSide -> centerSpawnX
                     RightSide -> rightSpawnX
          
          level = wsLevel (waveState world)
          -- Use level-scaled enemy creation
          enemyBase = Config.createEnemyWithLevel level (nextEntityId world) enemyType (spawnX, spawnY) spawnSide (timeElapsed world)
          -- Additional wave-based scaling within the level
          waveInLevel = wsWaveInLevel (waveState world)
          waveScale = 1.0 + 0.05 * fromIntegral waveInLevel
          enemy = enemyBase
            { enemyHP = enemyHP enemyBase * waveScale
            , enemyMaxHP = enemyMaxHP enemyBase * waveScale
            , enemyDamage = enemyDamage enemyBase * waveScale
            }

          enemies' = M.insert (nextEntityId world) enemy (enemies world)
      in world
        { enemies = enemies'
        , nextEntityId = nextEntityId world + 1
        }

selectEnemyType :: DirectorPlan -> StdGen -> (UnitType, StdGen)
selectEnemyType plan gen =
  let compList = M.toList (dpComposition plan)
      totalWeight = sum $ map snd compList
      (rand, gen1) = randomR (0, totalWeight - 1) gen
      selected = selectWeighted rand compList
  in (selected, gen1)

selectWeighted :: Int -> [(UnitType, Int)] -> UnitType
selectWeighted _ [] = GruntRaider
selectWeighted n ((ut, weight):rest) =
  if n < weight
  then ut
  else selectWeighted (n - weight) rest

selectSpawnSide :: DirectorPlan -> StdGen -> (SpawnSide, StdGen)
selectSpawnSide plan gen =
  let sides = dpSpawnSides plan
      (idx, gen1) = randomR (0, length sides - 1) gen
  in (sides !! idx, gen1)

-- ============================================================================
-- Helpers
-- ============================================================================

calculateEnemyCount :: Int -> Int -> Float -> Int
calculateEnemyCount level wave diffMult =
  let base = fromIntegral baseEnemyCount
      levelScale = fromIntegral level * enemyCountScaling
      waveScale = fromIntegral wave * 1.2
  in round $ (base + levelScale + waveScale) * diffMult

calculateBossWaveCount :: Int -> Float -> Int
calculateBossWaveCount level diffMult =
  let base = fromIntegral baseEnemyCount * 2.0
      levelScale = fromIntegral level * enemyCountScaling * 1.5
  in round $ (base + levelScale) * diffMult