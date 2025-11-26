module Config where

import Types
import Constants
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
  }

initialCastle :: Castle
initialCastle = Castle
  { castlePos = (castleX, castleY)
  , castleHP = Constants.castleMaxHP
  , Types.castleMaxHP = Constants.castleMaxHP
  , Types.castleSize = Constants.castleSize
  }

initialFort :: Fort
initialFort = Fort
  { fortWalls = initialWalls
  , fortGate = initialGate
  , fortBounds = ((fortLeft, fortBottom), (fortRight, fortTop))
  , fortInteriorDefences = []
  }

initialGate :: Gate
initialGate = Gate
  { gatePos = (gateX, gateY)
  , gateHP = Constants.gateMaxHP
  , Types.gateMaxHP = Constants.gateMaxHP
  , Types.gateWidth = Constants.gateWidth
  , gateDestroyed = False
  }

initialWalls :: [WallSegment]
initialWalls =
  [ WallSegment 0 (fortLeft, fortTop) (fortRight, fortTop) Constants.wallMaxHP Constants.wallMaxHP (Just (fortCenterX, fortTop))
  , WallSegment 1 (fortLeft, fortBottom) (fortLeft, gateY - Constants.gateWidth/2) Constants.wallMaxHP Constants.wallMaxHP Nothing
  , WallSegment 2 (fortLeft, gateY + Constants.gateWidth/2) (fortLeft, fortTop) Constants.wallMaxHP Constants.wallMaxHP Nothing
  , WallSegment 3 (fortLeft, fortBottom) (fortRight, fortBottom) Constants.wallMaxHP Constants.wallMaxHP (Just (fortCenterX, fortBottom))
  , WallSegment 4 (fortRight, fortBottom) (fortRight, fortTop) Constants.wallMaxHP Constants.wallMaxHP Nothing
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

leftPath :: [Vec2]
leftPath =
  [ (leftSpawnX, -200)
  , (-500, -150)
  , (-300, -100)
  , (-100, -50)
  , (gateX, gateY)
  ]

centerPath :: [Vec2]
centerPath =
  [ (centerSpawnX, 0)
  , (-300, 0)
  , (-150, 0)
  , (gateX, gateY)
  ]

rightPath :: [Vec2]
rightPath =
  [ (rightSpawnX, 200)
  , (-200, 150)
  , (-100, 100)
  , (50, 50)
  , (gateX, gateY)
  ]

initialInsideFortPaths :: [Vec2]
initialInsideFortPaths =
  [ (gateX + 50, gateY)
  , (fortCenterX - 50, gateY)
  , (fortCenterX, gateY)
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
      canClimb = False  -- No climbing enemies in new design
      prefs = unitTypePreferences ut
      initialAnim = AnimationState { animType = AnimMove, animFrame = 0, animTime = 0 }
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
    }

unitTypeToRole :: UnitType -> UnitRole
unitTypeToRole GruntRaider = Melee
unitTypeToRole BruteCrusher = Heavy
unitTypeToRole Direwolf = Fast
unitTypeToRole Shieldbearer = Heavy
unitTypeToRole Pyromancer = Ranged
unitTypeToRole Necromancer = Ranged
unitTypeToRole BoulderRamCrew = Siege
unitTypeToRole IronbackMinotaur = Boss
unitTypeToRole FireDrake = Boss
unitTypeToRole LichKingArcthros = Boss

unitTypePreferences :: UnitType -> [TargetPreference]
unitTypePreferences BoulderRamCrew = [PreferGate, PreferWalls, PreferCastle]
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
    , Types.towerMaxHP = maxHP
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

-- Generate random decorations scattered around grass (not on path or castle)
generateDecorations :: [Decoration]
generateDecorations =
  let
    -- Generate positions avoiding path and castle area
    positions = filter isValidDecoPosition [
      (-600, -200), (-500, 100), (-400, -300), (-300, 200),
      (-200, -100), (-100, 300), (0, -250), (100, 150),
      (200, -300), (300, 100), (500, -200), (600, 250),
      (-550, 0), (-450, -150), (-350, 250), (-250, -200),
      (-150, 50), (-50, -300), (50, 200), (150, -100),
      (250, 300), (350, -150), (450, 100), (550, -250)
      ]
    decoTypes = cycle [TreeSmall, TreeLarge, Bush, Rock]
  in zipWith (\id (pos, decoType) -> Decoration
    { decoId = id
    , decoType = decoType
    , decoPos = pos
    }) [2000..] (zip positions decoTypes)

isValidDecoPosition :: Vec2 -> Bool
isValidDecoPosition (x, y) =
  -- Not inside fort
  not (x >= fortLeft && x <= fortRight && y >= fortBottom && y <= fortTop) &&
  -- Not too close to spawn points
  not (x >= leftSpawnX - 100 && x <= leftSpawnX + 100) &&
  not (x >= centerSpawnX - 100 && x <= centerSpawnX + 100) &&
  not (x >= rightSpawnX - 100 && x <= rightSpawnX + 100) &&
  -- Not too close to castle
  distance (x, y) (castleX, castleY) > 200
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)