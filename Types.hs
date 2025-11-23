{-# LANGUAGE RecordWildCards #-}

-- Types.hs - Shared data types for Medieval Siege Simulator

module Types where

import Graphics.Gloss
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Position = (Float, Float)
type GridPos = (Int, Int)

data GameState = GameState
  { castle :: Castle
  , towers :: [Tower]
  , enemies :: [Enemy]
  , projectiles :: [Projectile]
  , particles :: [Particle]
  , resources :: Resources
  , wave :: Int
  , waveTimer :: Float
  , selectedTool :: Maybe TowerType
  , hoveredCell :: Maybe GridPos
  , gameOver :: Bool
  , victory :: Bool
  , aiMemory :: AIMemory
  , grid :: GameGrid
  , gameTime :: Float
  , notifications :: [Notification]
  , tutorialStep :: Int
  }

data Castle = Castle
  { castlePos :: Position
  , castleHealth :: Int
  , castleMaxHealth :: Int
  , castleShield :: Int
  }

data TowerType = Arrow | Cannon | Ice | Lightning deriving (Eq, Show)

data Tower = Tower
  { towerPos :: Position
  , towerType :: TowerType
  , towerCooldown :: Float
  , towerKills :: Int
  , towerLevel :: Int
  }

data EnemyType
  = BasicEnemy
  | FastEnemy
  | TankEnemy
  | BossEnemy
  deriving (Eq, Ord, Show)

data EnemyState = Advancing | Attacking | Retreating | Regrouping deriving (Eq)

data Enemy = Enemy
  { enemyPos :: Position
  , enemyHealth :: Int
  , enemyMaxHealth :: Int
  , enemySpeed :: Float
  , enemyPath :: [GridPos]
  , enemyStunned :: Float
  , enemyId :: Int
  , enemyType :: EnemyType
  , enemyState :: EnemyState
  , enemyTarget :: Maybe Int
  , enemyRetreatTimer :: Float
  , enemyArmor :: Int
  }

data Projectile = Projectile
  { projPos :: Position
  , projTarget :: Int
  , projDamage :: Int
  , projSpeed :: Float
  , projType :: TowerType
  , projLifetime :: Float
  }

data ParticleType 
  = CircleParticle 
  | TextParticle 
  | SparkParticle
  | SmokeParticle
  deriving (Eq)

data Particle = Particle
  { partPos :: Position
  , partVel :: (Float, Float)
  , partColor :: Color
  , partLife :: Float
  , partSize :: Float
  , partText :: Maybe String
  , partType :: ParticleType
  }

data Resources = Resources
  { gold :: Int
  , mana :: Int
  , maxMana :: Int
  }

data AIMemory = AIMemory
  { dangerZones :: Map.Map GridPos Float
  , successfulPaths :: [([GridPos], Float)]
  , towerThreatMap :: Map.Map GridPos Float
  , lastWaveResult :: WaveResult
  , adaptationLevel :: Int
  , enemyLosses :: Map.Map EnemyType Int
  }

data WaveResult = WaveResult
  { enemiesLost :: Int
  , damageDealt :: Int
  , timeToComplete :: Float
  } deriving (Show)

data GameGrid = GameGrid
  { gridWidth :: Int
  , gridHeight :: Int
  , obstacles :: Set.Set GridPos
  , terrain :: Map.Map GridPos TerrainType
  }

data TerrainType = Normal | Rough | Fortress deriving (Eq)

data Notification = Notification
  { notifText :: String
  , notifColor :: Color
  , notifLife :: Float
  , notifPos :: Float
  }