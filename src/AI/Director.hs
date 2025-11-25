module AI.Director where

import Types
import Constants
import qualified Data.Map.Strict as M
import qualified Data.List as L
import System.Random

-- ============================================================================
-- AI Director - Adaptive Difficulty
-- ============================================================================

planNextWave :: World -> StdGen -> (DirectorPlan, StdGen)
planNextWave world gen =
  let threat = threatData world
      level = wsLevel (waveState world)
      wave = wsWaveInLevel (waveState world)
      isBossWave = wave == wavesPerLevel
      
      (sides, gen1) = selectSpawnSides threat gen
      (composition, gen2) = selectComposition threat level wave isBossWave gen1
      diffMult = calculateDifficultyMultiplier threat level
      specialUnits = selectSpecialUnits threat level isBossWave
      
      plan = DirectorPlan
        { dpSpawnSides = sides
        , dpComposition = composition
        , dpDifficultyMult = diffMult
        , dpSpecialUnits = specialUnits
        }
  in (plan, gen2)

-- ============================================================================
-- Spawn Side Selection
-- ============================================================================

selectSpawnSides :: ThreatData -> StdGen -> ([SpawnSide], StdGen)
selectSpawnSides threat gen =
  let weakSides = tdWeakSides threat
      gateDmg = tdGateDamageRatio threat
      
      -- If gate is damaged, pressure it more
      primarySides = if gateDmg > 0.5
                     then [CenterSide, LeftSide]
                     else weakSides
      
      -- Add a random third side sometimes
      (shouldAddThird, gen1) = random gen :: (Bool, StdGen)
      (thirdSide, gen2) = randomR (0, 2) gen1
      
      allSides = if shouldAddThird
                 then primarySides ++ [toSpawnSide thirdSide]
                 else primarySides
  in (take 3 $ L.nub allSides, gen2)

toSpawnSide :: Int -> SpawnSide
toSpawnSide 0 = LeftSide
toSpawnSide 1 = CenterSide
toSpawnSide _ = RightSide

-- ============================================================================
-- Composition Selection
-- ============================================================================

selectComposition :: ThreatData -> Int -> Int -> Bool -> StdGen -> (M.Map UnitType Int, StdGen)
selectComposition threat level wave isBossWave gen
  | isBossWave = selectBossComposition threat level gen
  | otherwise = selectNormalComposition threat level wave gen

selectNormalComposition :: ThreatData -> Int -> Int -> StdGen -> (M.Map UnitType Int, StdGen)
selectNormalComposition threat level wave gen =
  let baseCount = baseEnemyCount + level * 5 + wave * 3
      towerComp = tdTowerComposition threat
      
      -- Counter player's strategy
      (grunts, casters, heavies, fast, siege) = calculateCounters towerComp baseCount
      
      comp = M.fromList
        [ (GruntRaider, grunts)
        , (Shieldbearer, grunts `div` 3)
        , (Direwolf, fast)
        , (Pyromancer, casters)
        , (BruteCrusher, heavies)
        , (BoulderRamCrew, siege)
        ]
  in (comp, gen)

selectBossComposition :: ThreatData -> Int -> StdGen -> (M.Map UnitType Int, StdGen)
selectBossComposition threat level gen =
  let baseCount = baseEnemyCount + level * 8
      
      -- Boss wave has stronger units based on level
      comp = M.fromList
        [ (IronbackMinotaur, if level == 3 then 1 else 0)
        , (FireDrake, if level == 6 then 1 else 0)
        , (LichKingArcthros, if level == 9 then 1 else 0)
        , (BruteCrusher, level * 2)
        , (Shieldbearer, level * 2)
        , (Pyromancer, level)
        , (Necromancer, if level >= 4 then level - 2 else 0)
        , (BoulderRamCrew, if level >= 5 then 1 else 0)
        ]
  in (comp, gen)

calculateCounters :: M.Map TowerType Int -> Int -> (Int, Int, Int, Int, Int)
calculateCounters towerComp baseCount =
  let arrowCount = M.findWithDefault 0 ArrowTower towerComp
      fireCount = M.findWithDefault 0 FireTower towerComp
      teslaCount = M.findWithDefault 0 TeslaTower towerComp
      
      -- More arrow towers → more shield bearers (counted as grunts)
      gruntBonus = arrowCount * 2
      
      -- More fire towers → more heavy armored units
      heavyBonus = fireCount * 2
      
      -- More tesla towers → more fast units
      fastBonus = teslaCount * 2
      
      grunts = baseCount `div` 2 + gruntBonus
      casters = baseCount `div` 6
      heavies = baseCount `div` 8 + heavyBonus
      fast = baseCount `div` 5 + fastBonus
      siege = baseCount `div` 10
  in (grunts, casters, heavies, fast, siege)

-- ============================================================================
-- Difficulty Multiplier
-- ============================================================================

calculateDifficultyMultiplier :: ThreatData -> Int -> Float
calculateDifficultyMultiplier threat level =
  let baseMultiplier = 1.0 + fromIntegral level * 0.2
      
      -- If player is rich, increase difficulty
      goldMult = if tdPlayerGold threat > 1000
                 then 1.3
                 else 1.0
      
      -- If castle is undamaged, increase difficulty
      castleMult = if tdCastleDamageRatio threat < 0.1
                   then 1.2
                   else 1.0
      
      -- If clearing waves fast, increase difficulty
      timeMult = if tdAverageClearTime threat < 30
                 then 1.25
                 else 1.0
  in baseMultiplier * goldMult * castleMult * timeMult

-- ============================================================================
-- Special Units Selection
-- ============================================================================

selectSpecialUnits :: ThreatData -> Int -> Bool -> [UnitType]
selectSpecialUnits threat level isBossWave =
  let trapCount = sum $ M.elems (tdTrapUsage threat)
      gateDmg = tdGateDamageRatio threat
      
      specialUnits = []
      
      -- Add direwolves if there are many traps (fast units to bypass)
      specialUnits1 = if trapCount > 5
                      then Direwolf : specialUnits
                      else specialUnits
      
      -- Add rams if gate is strong
      specialUnits2 = if gateDmg < 0.3 && level >= 2
                      then BoulderRamCrew : specialUnits1
                      else specialUnits1
      
      -- Add pyromancers if there are many towers
      specialUnits3 = if tdTowerDensity threat > 0.01 && level >= 3
                      then Pyromancer : specialUnits2
                      else specialUnits2
      
      -- Add necromancers at higher levels
      specialUnits4 = if level >= 4
                      then Necromancer : specialUnits3
                      else specialUnits3
  in specialUnits4