module Config where

import Types
import qualified Constants
import Constants (castleX, castleY, gateX, gateY, fortLeft, fortRight, fortTop, fortBottom, fortCenterX, startingGold, buildPhaseTime, leftSpawnX, centerSpawnX, rightSpawnX, towerStats, enemyStats)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- ============================================================================
-- Initial World Setup
-- ============================================================================

initialWorld :: World
initialWorld = World
  { castle = initialCastle
  , fort = initialFort
  , enemies = M.empty
  , towers = M.empty
  , traps = M.empty
  , projectiles = M.empty
  , decorations = initialDecorations
  , visualEffects = []
  , resources = initialResources
  , abilities = initialAbilities
  , waveState = initialWaveState
  , threatData = initialThreatData
  , directorPlan = Nothing
  , timeElapsed = 0
  , gameSpeed = Speed1x
  , isPaused = False
  , isGameOver = False
  , isVictory = False
  , renderMode = RenderSprites
  , inputState = initialInputState
  , showDebug = False
  , nextEntityId = 1000
  , paths = initialPaths
  , insideFortPaths = initialInsideFortPaths
  , upgradeUnlock = initialUpgradeUnlock
  , soundEvents = []
  , shouldExit = False
  , knownTraps = S.empty
  , gameMessage = Nothing
  }

initialCastle :: Castle
initialCastle = Castle
  { castlePos = (castleX, castleY)
  , castleHP = Constants.castleMaxHP
  , castleMaxHP = Constants.castleMaxHP
  , castleSize = Constants.castleSize
  }

initialFort :: Fort
initialFort = Fort
  { fortWalls = initialWalls
  , fortGates = initialGates  -- 3 gates now
  , fortBounds = ((fortLeft, fortBottom), (fortRight, fortTop))
  , fortInteriorDefences = []
  }

-- Create 3 gates: top, center, bottom
initialGates :: [Gate]
initialGates =
  [ Gate { gateId = 0, gatePos = (gateX, Constants.gate0Y), gateHP = Constants.gateMaxHP, gateMaxHP = Constants.gateMaxHP, gateWidth = Constants.gateWidth, gateDestroyed = False, gateLevel = 1 }
  , Gate { gateId = 1, gatePos = (gateX, Constants.gate1Y), gateHP = Constants.gateMaxHP, gateMaxHP = Constants.gateMaxHP, gateWidth = Constants.gateWidth, gateDestroyed = False, gateLevel = 1 }
  , Gate { gateId = 2, gatePos = (gateX, Constants.gate2Y), gateHP = Constants.gateMaxHP, gateMaxHP = Constants.gateMaxHP, gateWidth = Constants.gateWidth, gateDestroyed = False, gateLevel = 1 }
  ]

initialWalls :: [WallSegment]
initialWalls =
  -- Walls with gaps for 3 gates on the left side
  let gw = Constants.gateWidth / 2
      g0y = Constants.gate0Y
      g1y = Constants.gate1Y
      g2y = Constants.gate2Y
  in
  [ -- Top wall (horizontal)
    WallSegment 0 (fortLeft, fortTop) (fortRight, fortTop) Constants.wallMaxHP Constants.wallMaxHP (Just (fortCenterX, fortTop))
  -- Left wall segments (with gaps for 3 gates)
  , WallSegment 1 (fortLeft, fortBottom) (fortLeft, g2y - gw) Constants.wallMaxHP Constants.wallMaxHP (Just (fortLeft + 20, (fortBottom + g2y - gw) / 2))
  , WallSegment 2 (fortLeft, g2y + gw) (fortLeft, g1y - gw) Constants.wallMaxHP Constants.wallMaxHP Nothing
  , WallSegment 3 (fortLeft, g1y + gw) (fortLeft, g0y - gw) Constants.wallMaxHP Constants.wallMaxHP Nothing
  , WallSegment 4 (fortLeft, g0y + gw) (fortLeft, fortTop) Constants.wallMaxHP Constants.wallMaxHP (Just (fortLeft + 20, (g0y + gw + fortTop) / 2))
  -- Bottom wall (horizontal)
  , WallSegment 5 (fortLeft, fortBottom) (fortRight, fortBottom) Constants.wallMaxHP Constants.wallMaxHP (Just (fortCenterX, fortBottom))
  -- Right wall (vertical)
  , WallSegment 6 (fortRight, fortBottom) (fortRight, fortTop) Constants.wallMaxHP Constants.wallMaxHP (Just (fortRight - 20, 0))
  ]

initialResources :: Resources
initialResources = Resources
  { resGold = startingGold
  , resIncome = 0
  , resTotalEarned = startingGold
  , resTotalSpent = 0
  }

initialAbilities :: M.Map AbilityType AbilityState
initialAbilities = M.fromList
  [ (Firestorm, AbilityState Firestorm 0 0 False)
  , (FreezeField, AbilityState FreezeField 0 0 False)
  , (RepairWalls, AbilityState RepairWalls 0 0 False)
  , (TimeSlow, AbilityState TimeSlow 0 0 False)
  ]

initialWaveState :: WaveState
initialWaveState = WaveState
  { wsLevel = 1
  , wsWaveInLevel = 0
  , wsPhase = BuildPhase buildPhaseTime
  , wsEnemiesSpawned = 0
  , wsEnemiesToSpawn = 0
  , wsSpawnTimer = 0
  , wsWaveCleared = False
  , wsLevelCleared = False
  , wsGateRepairPending = False
  }

initialThreatData :: ThreatData
initialThreatData = ThreatData
  { tdTowerComposition = M.empty
  , tdTowerDensity = 0
  , tdWeakSides = [LeftSide, CenterSide, RightSide]
  , tdMostDamagingTowers = []
  , tdTrapUsage = M.empty
  , tdGateDamageRatio = 0
  , tdCastleDamageRatio = 0
  , tdAverageClearTime = 0
  , tdPlayerGold = startingGold
  , tdLastWaveResult = WaveInProgress
  }

initialInputState :: InputState
initialInputState = InputState
  { mousePos = (0, 0)
  , mouseWorldPos = (0, 0)
  , mouseClicked = False
  , hoveredTile = Nothing
  , buildMode = NoBuild
  , selectedTower = Nothing
  }

-- ============================================================================
-- Pathfinding Setup
-- ============================================================================

initialPaths :: M.Map SpawnSide [Vec2]
initialPaths = M.fromList
  [ (LeftSide, leftPath)
  , (CenterSide, centerPath)
  , (RightSide, rightPath)
  ]

-- Path to bottom gate (gate 2)
leftPath :: [Vec2]
leftPath =
  [ (leftSpawnX, -200)
  , (-500, -180)
  , (-300, Constants.gate2Y)
  , (-100, Constants.gate2Y)
  , (gateX, Constants.gate2Y)  -- Bottom gate
  ]

-- Path to center gate (gate 1)
centerPath :: [Vec2]
centerPath =
  [ (centerSpawnX, 0)
  , (-300, 0)
  , (-150, Constants.gate1Y)
  , (gateX, Constants.gate1Y)  -- Center gate
  ]

-- Path to top gate (gate 0)
rightPath :: [Vec2]
rightPath =
  [ (rightSpawnX, 200)
  , (-200, 180)
  , (-100, Constants.gate0Y)
  , (gateX, Constants.gate0Y)  -- Top gate
  ]

-- Inside fort paths - converge toward castle from any gate
initialInsideFortPaths :: [Vec2]
initialInsideFortPaths =
  [ (fortCenterX - 100, 0)  -- Move toward center
  , (fortCenterX, 0)
  , (castleX - 100, castleY)
  , (castleX, castleY)
  ]

-- ============================================================================
-- Enemy Configuration Helpers
-- ============================================================================

createEnemy :: EntityId -> UnitType -> Vec2 -> SpawnSide -> Float -> Enemy
createEnemy eid ut pos side time =
  let (hp, armor, spd, range, dmg, cd) = enemyStats ut
      role = unitTypeToRole ut
      -- Wall climbers: WallClimber, Berserker, and Assassin can all climb walls
      canClimb = ut `elem` [WallClimber, Berserker, Assassin]
      prefs = unitTypePreferences ut
      initialAnim = AnimationState { animType = AnimMove, animFrame = 0, animTime = 0 }
      -- Assign target gate based on spawn side
      targetGate = case side of
        LeftSide -> 2   -- Bottom gate
        CenterSide -> 1 -- Center gate
        RightSide -> 0  -- Top gate
      -- Generate pseudo-random attack offset based on entity ID
      -- This spreads enemies around the gate instead of stacking
      offsetX = fromIntegral ((eid * 17) `mod` 60) - 30  -- -30 to +30
      offsetY = fromIntegral ((eid * 31) `mod` 80) - 40  -- -40 to +40
  in Enemy
    { enemyId = eid
    , enemyType = ut
    , enemyRole = role
    , enemyPos = pos
    , enemyVel = (0, 0)
    , enemyHP = hp
    , enemyMaxHP = hp
    , enemyArmor = armor
    , enemySpeed = spd
    , enemyAttackRange = range
    , enemyDamage = dmg
    , enemyAIState = MovingToFort
    , enemyPathIndex = 0
    , enemyTargetPrefs = prefs
    , enemyCanClimb = canClimb
    , enemyFireResist = 0
    , enemyIceResist = 0
    , enemySlowFactor = 1.0
    , enemyBurnDuration = 0
    , enemyLastAttackTime = time
    , enemyAttackCooldown = cd
    , enemyHitFlash = 0
    , enemySpawnSide = side
    , enemyAnimState = initialAnim
    , enemyDeathTimer = 0
    , bossAbilityCooldown = if role == Boss then 0 else 999.0
    , bossLastAbilityTime = time
    , bossSpawnTimer = time
    , enemyTargetGate = targetGate
    , enemyAttackOffset = (offsetX, offsetY)
    }

unitTypeToRole :: UnitType -> UnitRole
unitTypeToRole GruntRaider = Melee
unitTypeToRole BruteCrusher = Heavy
unitTypeToRole Direwolf = Fast
unitTypeToRole Shieldbearer = Heavy
unitTypeToRole Pyromancer = Ranged
unitTypeToRole Necromancer = Ranged
unitTypeToRole TrapBreaker = Fast
unitTypeToRole WallClimber = Fast
unitTypeToRole Berserker = Melee   -- Short-range high damage
unitTypeToRole Assassin = Fast     -- Fast short-range
unitTypeToRole BoulderRamCrew = Siege
unitTypeToRole IronbackMinotaur = Boss
unitTypeToRole FireDrake = Boss
unitTypeToRole LichKingArcthros = Boss

unitTypePreferences :: UnitType -> [TargetPreference]
unitTypePreferences BoulderRamCrew = [PreferGate, PreferWalls, PreferCastle]
unitTypePreferences TrapBreaker = [PreferTraps, PreferGate, PreferCastle]
unitTypePreferences WallClimber = [PreferWalls, PreferTowers, PreferCastle]  -- Climb wall, then attack towers
unitTypePreferences Berserker = [PreferWalls, PreferTowers, PreferCastle]    -- Climb wall, siege defenses
unitTypePreferences Assassin = [PreferWalls, PreferTowers, PreferCastle]     -- Climb wall, target towers
unitTypePreferences Pyromancer = [PreferTowers, PreferCastle]
unitTypePreferences Necromancer = [PreferTowers, PreferCastle]
unitTypePreferences FireDrake = [PreferTowers, PreferCastle]
unitTypePreferences LichKingArcthros = [PreferTowers, PreferCastle]
unitTypePreferences _ = [PreferGate, PreferWalls, PreferCastle]

-- ============================================================================
-- Tower Configuration Helpers
-- ============================================================================

createTower :: EntityId -> TowerType -> Vec2 -> Float -> Tower
createTower tid tt pos time =
  let (range, dmg, fr) = towerStats tt
      maxHP = Constants.towerMaxHP tt
      initialAnim = AnimationState { animType = AnimIdle, animFrame = 0, animTime = 0 }
  in Tower
    { towerId = tid
    , towerType = tt
    , towerPos = pos
    , towerLevel = 1
    , towerRange = range
    , towerDamage = dmg
    , towerFireRate = fr
    , towerLastFireTime = time
    , towerTargetId = Nothing
    , towerKills = 0
    , towerDamageDealt = 0
    , towerHP = maxHP
    , towerMaxHP = maxHP
    , towerAnimState = initialAnim
    , towerDeathTimer = 0
    }

-- ============================================================================
-- Upgrade Unlock Initialization
-- ============================================================================

initialUpgradeUnlock :: UpgradeUnlock
initialUpgradeUnlock = UpgradeUnlock
  { upgradeLevel = 3
  , upgradeCost = 100
  , upgradeUnlocked = False
  }

-- ============================================================================
-- Decoration Initialization
-- ============================================================================

initialDecorations :: M.Map EntityId Decoration
initialDecorations = M.fromList $ zip [1..] $ generateDecorations

-- Generate many decorations scattered around grass (not on path or castle)
-- Creates a lush, forested environment around the battlefield
generateDecorations :: [Decoration]
generateDecorations =
  let
    -- Dense tree/bush positions for outer areas (far from fort and paths)
    -- Top edge forest
    topForest = [(x, y) | x <- [-750, -650 .. 750], y <- [350, 400]]
    -- Bottom edge forest
    bottomForest = [(x, y) | x <- [-750, -650 .. 750], y <- [-350, -400]]
    -- Left edge forest (spawn area backdrop)
    leftForest = [(x, y) | x <- [-780, -730], y <- [-300, -200 .. 300]]
    -- Scattered trees/bushes in battlefield (avoiding paths)
    scatteredTrees = [
      -- Upper left area
      (-600, 250), (-550, 300), (-500, 280), (-450, 320),
      (-650, 180), (-580, 200), (-520, 220), (-470, 180),
      -- Lower left area
      (-600, -250), (-550, -300), (-500, -280), (-450, -320),
      (-650, -180), (-580, -200), (-520, -220), (-470, -180),
      -- Upper middle (between paths)
      (-350, 280), (-300, 320), (-250, 300), (-200, 280),
      (-380, 220), (-320, 250), (-280, 200),
      -- Lower middle (between paths)
      (-350, -280), (-300, -320), (-250, -300), (-200, -280),
      (-380, -220), (-320, -250), (-280, -200),
      -- Far right (behind fort, visible edges)
      (700, 300), (750, 250), (680, 350), (720, 400),
      (700, -300), (750, -250), (680, -350), (720, -400),
      -- Additional scattered bushes
      (-620, 100), (-570, 50), (-530, -50), (-480, -100),
      (-420, 150), (-370, 100), (-330, 50),
      (-420, -150), (-370, -100), (-330, -50),
      -- Near top/bottom edges
      (-150, 380), (-50, 400), (50, 380), (150, 400),
      (-150, -380), (-50, -400), (50, -380), (150, -400),
      -- More bushes for density
      (-680, 0), (-640, 50), (-640, -50),
      (-590, 120), (-590, -120),
      (-540, 0), (-540, 80), (-540, -80),
      -- Rocks scattered
      (-700, 150), (-700, -150), (-650, 0),
      (-400, 350), (-400, -350),
      (-100, 350), (-100, -350),
      (650, 200), (650, -200)
      ]
    
    -- Combine all positions and filter valid ones
    allPositions = filter isValidDecoPosition (topForest ++ bottomForest ++ leftForest ++ scatteredTrees)
    
    -- Assign decoration types based on position for natural look
    assignDecoType :: Vec2 -> DecoType
    assignDecoType (x, y)
      | abs y > 350 = TreeLarge  -- Edge forests have large trees
      | x < -700 = TreeLarge     -- Left edge has large trees
      | abs y > 250 = TreeSmall  -- Mid-outer areas have small trees
      | abs x > 600 = TreeSmall  -- Far areas have small trees
      | (round x + round y) `mod` 3 == 0 = Bush  -- Some bushes
      | (round x + round y) `mod` 5 == 0 = Rock  -- Some rocks
      | otherwise = Bush         -- Default to bushes
    
  in zipWith (\id pos -> Decoration
    { decoId = id
    , decoType = assignDecoType pos
    , decoPos = pos
    }) [2000..] allPositions

isValidDecoPosition :: Vec2 -> Bool
isValidDecoPosition (x, y) =
  -- Not inside fort (with some margin)
  not (x >= fortLeft - 30 && x <= fortRight + 30 && y >= fortBottom - 30 && y <= fortTop + 30) &&
  -- Not too close to left spawn point
  not (x >= leftSpawnX - 50 && x <= leftSpawnX + 80 && abs y < 250) &&
  -- Not too close to center spawn point
  not (x >= centerSpawnX - 50 && x <= centerSpawnX + 80 && abs y < 100) &&
  -- Not too close to right spawn point
  not (x >= rightSpawnX - 50 && x <= rightSpawnX + 80 && abs y < 250) &&
  -- Not on main paths (approximate path areas)
  not (isOnPath (x, y)) &&
  -- Not too close to castle
  distance (x, y) (castleX, castleY) > 250
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
    
    -- Check if position is on or near enemy paths
    isOnPath (px, py) =
      -- Left path area
      (px > leftSpawnX && px < gateX && abs (py - leftPathY px) < 80) ||
      -- Center path area  
      (px > centerSpawnX && px < gateX && abs py < 60) ||
      -- Right path area
      (px > rightSpawnX && px < gateX && abs (py - rightPathY px) < 80)
    
    -- Approximate Y position of left path at given X
    leftPathY px = -200 + (px - leftSpawnX) * 200 / (gateX - leftSpawnX)
    
    -- Approximate Y position of right path at given X
    rightPathY px = 200 - (px - rightSpawnX) * 200 / (gateX - rightSpawnX)