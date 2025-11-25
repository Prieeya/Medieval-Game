module AI.ThreatAnalysis where

import Types
import Constants
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- ============================================================================
-- Threat Analysis System
-- ============================================================================

analyzeThreat :: World -> ThreatData
analyzeThreat world =
  let composition = analyzeTowerComposition world
      density = analyzeTowerDensity world
      weakSides = analyzeWeakSides world
      damagingTowers = analyzeMostDamagingTowers world
      trapUsage = analyzeTrapUsage world
      gateDmg = analyzeGateDamage world
      castleDmg = analyzeCastleDamage world
      clearTime = estimateClearTime world
      gold = resGold (resources world)
      lastResult = tdLastWaveResult (threatData world)
  in ThreatData
    { tdTowerComposition = composition
    , tdTowerDensity = density
    , tdWeakSides = weakSides
    , tdMostDamagingTowers = damagingTowers
    , tdTrapUsage = trapUsage
    , tdGateDamageRatio = gateDmg
    , tdCastleDamageRatio = castleDmg
    , tdAverageClearTime = clearTime
    , tdPlayerGold = gold
    , tdLastWaveResult = lastResult
    }

-- ============================================================================
-- Tower Composition Analysis
-- ============================================================================

analyzeTowerComposition :: World -> M.Map TowerType Int
analyzeTowerComposition world =
  let towerList = M.elems (towers world)
      types = map towerType towerList
  in foldr (\tt acc -> M.insertWith (+) tt 1 acc) M.empty types

analyzeTowerDensity :: World -> Float
analyzeTowerDensity world =
  let towerCount = M.size (towers world)
      fortArea = fortWidth * fortHeight
  in fromIntegral towerCount / fortArea

-- ============================================================================
-- Weak Side Analysis
-- ============================================================================

analyzeWeakSides :: World -> [SpawnSide]
analyzeWeakSides world =
  let leftDensity = sideTowerDensity LeftSide world
      centerDensity = sideTowerDensity CenterSide world
      rightDensity = sideTowerDensity RightSide world
      allSides = [(LeftSide, leftDensity), (CenterSide, centerDensity), (RightSide, rightDensity)]
      sorted = L.sortBy (\(_, d1) (_, d2) -> compare d1 d2) allSides
  in map fst $ take 2 sorted

sideTowerDensity :: SpawnSide -> World -> Float
sideTowerDensity side world =
  let towerList = M.elems (towers world)
      (yMin, yMax) = case side of
                       LeftSide -> (-fortTop, -fortTop/3)
                       CenterSide -> (-fortTop/3, fortTop/3)
                       RightSide -> (fortTop/3, fortTop)
      towersInSide = filter (\t -> let (_, y) = towerPos t
                                   in y >= yMin && y <= yMax) towerList
  in fromIntegral (length towersInSide)

-- ============================================================================
-- Damaging Tower Analysis
-- ============================================================================

analyzeMostDamagingTowers :: World -> [TowerType]
analyzeMostDamagingTowers world =
  let towerList = M.elems (towers world)
      byType = L.groupBy (\t1 t2 -> towerType t1 == towerType t2) $
               L.sortBy (\t1 t2 -> compare (towerType t1) (towerType t2)) towerList
      totalDamageByType = map (\grp -> (towerType (head grp), sum (map towerDamageDealt grp))) byType
      sorted = L.sortBy (\(_, d1) (_, d2) -> compare d2 d1) totalDamageByType
  in map fst sorted

-- ============================================================================
-- Trap Usage Analysis
-- ============================================================================

analyzeTrapUsage :: World -> M.Map TrapType Int
analyzeTrapUsage world =
  let trapList = M.elems (traps world)
      types = map trapType trapList
  in foldr (\tt acc -> M.insertWith (+) tt 1 acc) M.empty types

-- ============================================================================
-- Gate & Castle Damage
-- ============================================================================

analyzeGateDamage :: World -> Float
analyzeGateDamage world =
  let gate = fortGate (fort world)
      currentHP = gateHP gate
      maxHP = Types.gateMaxHP gate
  in 1.0 - (currentHP / maxHP)

analyzeCastleDamage :: World -> Float
analyzeCastleDamage world =
  let c = castle world
      currentHP = castleHP c
      maxHP = Types.castleMaxHP c
  in 1.0 - (currentHP / maxHP)

-- ============================================================================
-- Clear Time Estimation
-- ============================================================================

estimateClearTime :: World -> Float
estimateClearTime world =
  -- Simple heuristic based on current wave state
  case wsPhase (waveState world) of
    InWave -> timeElapsed world
    _ -> tdAverageClearTime (threatData world)