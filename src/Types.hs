{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics (Generic)

-- ============================================================================
-- Core Types
-- ============================================================================

type Vec2 = (Float, Float)
type EntityId = Int

-- ============================================================================
-- Enemy Types
-- ============================================================================

data UnitRole = Melee | Fast | Ranged | Heavy | Siege | Boss
  deriving (Show, Eq, Ord, Generic)

data UnitType
  = GruntRaider      -- Basic Enemy
  | BruteCrusher     -- Tank Enemy
  | Direwolf         -- Fast Runner
  | Shieldbearer     -- Armored Unit
  | Pyromancer       -- Ranged Caster
  | Necromancer      -- Summoner
  | BoulderRamCrew   -- Siege Unit
  | IronbackMinotaur -- Boss Level 3
  | FireDrake        -- Boss Level 6
  | LichKingArcthros -- Boss Level 9
  deriving (Show, Eq, Ord, Generic)

data EnemyAIState
  = MovingToFort
  | AttackingGate
  | AttackingWall Int
  | ClimbingWall Int
  | InsideFort
  | AttackingTower EntityId
  | AttackingCastle
  | Dead
  deriving (Show, Eq, Generic)

data TargetPreference
  = PreferGate
  | PreferWalls
  | PreferTowers
  | PreferCastle
  | PreferTraps
  deriving (Show, Eq, Generic)

data Enemy = Enemy
  { enemyId :: EntityId
  , enemyType :: UnitType
  , enemyRole :: UnitRole
  , enemyPos :: Vec2
  , enemyVel :: Vec2
  , enemyHP :: Float
  , enemyMaxHP :: Float
  , enemyArmor :: Float
  , enemySpeed :: Float
  , enemyAttackRange :: Float
  , enemyDamage :: Float
  , enemyAIState :: EnemyAIState
  , enemyPathIndex :: Int
  , enemyTargetPrefs :: [TargetPreference]
  , enemyCanClimb :: Bool
  , enemyFireResist :: Float
  , enemyIceResist :: Float
  , enemySlowFactor :: Float
  , enemyBurnDuration :: Float
  , enemyLastAttackTime :: Float
  , enemyAttackCooldown :: Float
  , enemyHitFlash :: Float
  , enemySpawnSide :: SpawnSide
  } deriving (Show, Generic)

data SpawnSide = LeftSide | CenterSide | RightSide
  deriving (Show, Eq, Ord, Generic)

-- ============================================================================
-- Tower Types
-- ============================================================================

data TowerType
  = ArrowTower      -- Basic Defense Tower
  | CatapultTower   -- Siege Tower
  | CrossbowTower   -- Sniper Tower
  | FireTower       -- Burning Tower
  | TeslaTower      -- Lightning Tower
  | BallistaTower   -- Armor-Piercing Tower
  | PoisonTower     -- Debuff Tower
  | BombardTower    -- Cannon Tower
  deriving (Show, Eq, Ord, Generic)

data Tower = Tower
  { towerId :: EntityId
  , towerType :: TowerType
  , towerPos :: Vec2
  , towerLevel :: Int
  , towerRange :: Float
  , towerDamage :: Float
  , towerFireRate :: Float
  , towerLastFireTime :: Float
  , towerTargetId :: Maybe EntityId
  , towerKills :: Int
  , towerDamageDealt :: Float
  , towerHP :: Float
  , towerMaxHP :: Float
  } deriving (Show, Generic)

-- ============================================================================
-- Trap Types
-- ============================================================================

data TrapType
  = SpikeTrap        -- Cheap early damage
  | FreezeTrap       -- Stop fast units
  | FirePitTrap      -- Continuous AoE
  | MagicSnareTrap   -- Immobilize enemy
  | ExplosiveBarrel  -- Player-triggered burst
  deriving (Show, Eq, Ord, Generic)

data Trap = Trap
  { trapId :: EntityId
  , trapType :: TrapType
  , trapPos :: Vec2
  , trapTriggered :: Bool
  , trapActiveTime :: Float
  , trapAffectedEnemies :: S.Set EntityId
  } deriving (Show, Generic)

-- ============================================================================
-- Interior Defences
-- ============================================================================

data InteriorDefence
  = Barricade
      { barricadeId :: EntityId
      , barricadePos :: Vec2
      , barricadeHP :: Float
      , barricadeMaxHP :: Float
      }
  | InnerBallista
      { ballistaId :: EntityId
      , ballistaPos :: Vec2
      , ballistaLastFire :: Float
      }
  | MageCircle
      { mageCircleId :: EntityId
      , mageCirclePos :: Vec2
      , mageCircleLastCast :: Float
      }
  deriving (Show, Generic)

-- ============================================================================
-- Projectile Types
-- ============================================================================

data ProjectileType
  = Arrow
  | BallistaBolt
  | Fireball
  | IceShard
  | LightningBolt
  | BarrageShot
  | CatapultRock
  deriving (Show, Eq, Generic)

data Projectile = Projectile
  { projectileId :: EntityId
  , projectileType :: ProjectileType
  , projectilePos :: Vec2
  , projectileVel :: Vec2
  , projectileDamage :: Float
  , projectileTargetId :: Maybe EntityId
  , projectileSourceId :: EntityId
  , projectileLifetime :: Float
  , projectilePiercing :: Bool
  , projectileAoERadius :: Float
  , projectileHitEnemies :: S.Set EntityId
  } deriving (Show, Generic)

-- ============================================================================
-- Fort & Castle
-- ============================================================================

data WallSegment = WallSegment
  { wallId :: Int
  , wallStart :: Vec2
  , wallEnd :: Vec2
  , wallHP :: Float
  , wallMaxHP :: Float
  , wallClimbPoint :: Maybe Vec2
  } deriving (Show, Generic)

data Gate = Gate
  { gatePos :: Vec2
  , gateHP :: Float
  , gateMaxHP :: Float
  , gateWidth :: Float
  , gateDestroyed :: Bool
  } deriving (Show, Generic)

data Fort = Fort
  { fortWalls :: [WallSegment]
  , fortGate :: Gate
  , fortBounds :: (Vec2, Vec2)
  , fortInteriorDefences :: [InteriorDefence]
  } deriving (Show, Generic)

data Castle = Castle
  { castlePos :: Vec2
  , castleHP :: Float
  , castleMaxHP :: Float
  , castleSize :: Float
  } deriving (Show, Generic)

-- ============================================================================
-- Resources & Economy
-- ============================================================================

data Resources = Resources
  { resGold :: Int
  , resIncome :: Int
  , resTotalEarned :: Int
  , resTotalSpent :: Int
  } deriving (Show, Generic)

-- ============================================================================
-- Abilities
-- ============================================================================

data AbilityType
  = Firestorm
  | FreezeField
  | RepairWalls
  | TimeSlow
  deriving (Show, Eq, Ord, Generic)

data AbilityState = AbilityState
  { abilityType :: AbilityType
  , abilityCooldown :: Float
  , abilityDuration :: Float
  , abilityActive :: Bool
  } deriving (Show, Generic)

-- ============================================================================
-- Wave & Level System
-- ============================================================================

data WavePhase
  = InWave
  | BuildPhase Float
  | BossIncoming Float
  deriving (Show, Eq, Generic)

data WaveState = WaveState
  { wsLevel :: Int
  , wsWaveInLevel :: Int
  , wsPhase :: WavePhase
  , wsEnemiesSpawned :: Int
  , wsEnemiesToSpawn :: Int
  , wsSpawnTimer :: Float
  , wsWaveCleared :: Bool
  , wsLevelCleared :: Bool
  } deriving (Show, Generic)

-- ============================================================================
-- Threat Analysis & Director
-- ============================================================================

data ThreatData = ThreatData
  { tdTowerComposition :: M.Map TowerType Int
  , tdTowerDensity :: Float
  , tdWeakSides :: [SpawnSide]
  , tdMostDamagingTowers :: [TowerType]
  , tdTrapUsage :: M.Map TrapType Int
  , tdGateDamageRatio :: Float
  , tdCastleDamageRatio :: Float
  , tdAverageClearTime :: Float
  , tdPlayerGold :: Int
  , tdLastWaveResult :: WaveResult
  } deriving (Show, Generic)

data WaveResult
  = WaveVictory Float
  | WaveDefeat
  | WaveInProgress
  deriving (Show, Eq, Generic)

data DirectorPlan = DirectorPlan
  { dpSpawnSides :: [SpawnSide]
  , dpComposition :: M.Map UnitType Int
  , dpDifficultyMult :: Float
  , dpSpecialUnits :: [UnitType]
  } deriving (Show, Generic)

-- ============================================================================
-- Visual Effects
-- ============================================================================

data VisualEffect
  = ImpactFlash Vec2 Float Float
  | ExplosionEffect Vec2 Float Float
  | FireBurst Vec2 Float
  | TarSplash Vec2 Float
  | SpikePopup Vec2 Float
  | GateFlash Float
  | CastleFlash Float
  | DamageNumber Vec2 Float Float
  | EnemyAttackParticle Vec2 Vec2 Float  -- from, to, lifetime
  deriving (Show, Generic)

-- ============================================================================
-- Input State
-- ============================================================================

data BuildMode
  = PlaceTower TowerType
  | PlaceTrap TrapType
  | UpgradeMode
  | NoBuild
  deriving (Show, Eq, Generic)

data InputState = InputState
  { mousePos :: Vec2
  , mouseWorldPos :: Vec2
  , mouseClicked :: Bool
  , hoveredTile :: Maybe Vec2
  , buildMode :: BuildMode
  , selectedTower :: Maybe EntityId
  } deriving (Show, Generic)

-- ============================================================================
-- Rendering
-- ============================================================================

data RenderMode = RenderShapes | RenderSprites
  deriving (Show, Eq, Generic)

type SpriteId = String

data Assets = Assets
  { sprites :: M.Map SpriteId String
  } deriving (Show, Generic)

-- ============================================================================
-- Upgrade System
-- ============================================================================

data UpgradeUnlock = UpgradeUnlock
  { upgradeLevel :: Int        -- Level required to unlock
  , upgradeCost :: Int         -- Gold cost to unlock
  , upgradeUnlocked :: Bool    -- Whether player has unlocked it
  } deriving (Show, Generic)

data GameSpeed = Speed1x | Speed2x | Speed4x
  deriving (Show, Eq, Generic)

gameSpeedValue :: GameSpeed -> Float
gameSpeedValue Speed1x = 1.0
gameSpeedValue Speed2x = 2.0
gameSpeedValue Speed4x = 4.0

-- ============================================================================
-- World State
-- ============================================================================

data World = World
  { castle :: Castle
  , fort :: Fort
  , enemies :: M.Map EntityId Enemy
  , towers :: M.Map EntityId Tower
  , traps :: M.Map EntityId Trap
  , projectiles :: M.Map EntityId Projectile
  , visualEffects :: [VisualEffect]
  , resources :: Resources
  , abilities :: M.Map AbilityType AbilityState
  , waveState :: WaveState
  , threatData :: ThreatData
  , directorPlan :: Maybe DirectorPlan
  , timeElapsed :: Float
  , gameSpeed :: GameSpeed
  , isPaused :: Bool
  , isGameOver :: Bool
  , isVictory :: Bool
  , renderMode :: RenderMode
  , inputState :: InputState
  , showDebug :: Bool
  , nextEntityId :: EntityId
  , paths :: M.Map SpawnSide [Vec2]
  , insideFortPaths :: [Vec2]
  , upgradeUnlock :: UpgradeUnlock
  } deriving (Show, Generic)