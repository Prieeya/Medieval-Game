module Rendering.SpriteAnimation where

import Graphics.Gloss
import Types
import qualified Assets
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.FilePath ((</>))

-- Animation state for entities
data AnimationState = AnimationState
  { animCurrentFrame :: Int
  , animFrameCount :: Int
  , animTimeAccumulator :: Float
  , animFPS :: Float  -- Frames per second for animation
  }

-- Animation type
data AnimationType
  = AnimIdle
  | AnimAttack
  | AnimDeath
  | AnimMove
  deriving (Eq, Show)

-- Sprite cache for loaded images
{-# NOINLINE spriteCacheRef #-}
spriteCacheRef :: IORef (Map.Map String Picture)
spriteCacheRef = unsafePerformIO $ do
  Assets.ensureAssets
  ref <- newIORef Map.empty
  return ref

-- Get or load a sprite frame
getSpriteFrame :: String -> FilePath -> Picture -> Picture
getSpriteFrame key fp fallback = unsafePerformIO $ do
  m <- readIORef spriteCacheRef
  case Map.lookup key m of
    Just p -> return p
    Nothing -> do
      mp <- loadJuicyPNG fp
      let p = case mp of
                Just pic -> pic
                Nothing -> fallback
      writeIORef spriteCacheRef (Map.insert key p m)
      return p

-- Get animation frame number based on time
getAnimationFrame :: Float -> Int -> Float -> Int
getAnimationFrame timeAccumulator frameCount fps =
  let frameTime = 1.0 / fps
      frameIndex = floor (timeAccumulator / frameTime) `mod` frameCount
  in frameIndex + 1  -- Frames are 1-indexed (frame_01, frame_02, etc.)

-- Get sprite path for unit type
unitTypeToSpriteName :: UnitType -> String
unitTypeToSpriteName GruntRaider = "grunt_raider"
unitTypeToSpriteName BruteCrusher = "brute_crusher"
unitTypeToSpriteName Direwolf = "direwolf"
unitTypeToSpriteName Shieldbearer = "shieldbearer"
unitTypeToSpriteName Pyromancer = "pyromancer"
unitTypeToSpriteName Necromancer = "necromancer"
unitTypeToSpriteName BoulderRamCrew = "boulder_ram_crew"
unitTypeToSpriteName IronbackMinotaur = "ironback_minotaur"
unitTypeToSpriteName FireDrake = "fire_drake"
unitTypeToSpriteName LichKingArcthros = "lich_king_arcthros"

-- Get sprite path for tower type
towerTypeToSpriteName :: TowerType -> String
towerTypeToSpriteName ArrowTower = "arrow_tower"
towerTypeToSpriteName CatapultTower = "catapult_tower"
towerTypeToSpriteName CrossbowTower = "crossbow_tower"
towerTypeToSpriteName FireTower = "fire_tower"
towerTypeToSpriteName TeslaTower = "tesla_tower"
towerTypeToSpriteName BallistaTower = "ballista_tower"
towerTypeToSpriteName PoisonTower = "poison_tower"
towerTypeToSpriteName BombardTower = "bombard_tower"

-- Get sprite path for trap type
trapTypeToSpriteName :: TrapType -> String
trapTypeToSpriteName SpikeTrap = "spike_trap"
trapTypeToSpriteName FreezeTrap = "freeze_trap"
trapTypeToSpriteName FirePitTrap = "fire_pit_trap"
trapTypeToSpriteName MagicSnareTrap = "magic_snare_trap"
trapTypeToSpriteName ExplosiveBarrel = "explosive_barrel"

-- Get animation frame count (default values)
getAnimationFrameCount :: AnimationType -> Int
getAnimationFrameCount AnimIdle = 3
getAnimationFrameCount AnimAttack = 5
getAnimationFrameCount AnimDeath = 5
getAnimationFrameCount AnimMove = 6

-- Get animation FPS
getAnimationFPS :: AnimationType -> Float
getAnimationFPS AnimIdle = 2.0
getAnimationFPS AnimAttack = 8.0
getAnimationFPS AnimDeath = 6.0
getAnimationFPS AnimMove = 8.0

-- Determine animation type based on enemy state
enemyStateToAnimation :: EnemyAIState -> AnimationType
enemyStateToAnimation Dead = AnimDeath
enemyStateToAnimation AttackingGate = AnimAttack
enemyStateToAnimation (AttackingTower _) = AnimAttack
enemyStateToAnimation AttackingCastle = AnimAttack
enemyStateToAnimation MovingToFort = AnimMove
enemyStateToAnimation InsideFort = AnimMove
enemyStateToAnimation (AttackingWall _) = AnimAttack
enemyStateToAnimation (ClimbingWall _) = AnimMove

-- Determine animation type for tower
towerStateToAnimation :: Float -> Float -> AnimationType
towerStateToAnimation lastFireTime currentTime =
  let timeSinceFire = currentTime - lastFireTime
  in if timeSinceFire < 0.5  -- Attack animation for 0.5 seconds after firing
     then AnimAttack
     else AnimIdle

-- Render animated sprite for enemy
renderAnimatedEnemySprite :: UnitType -> EnemyAIState -> Float -> Picture -> Picture
renderAnimatedEnemySprite unitType state currentTime fallback =
  let animType = enemyStateToAnimation state
      spriteName = unitTypeToSpriteName unitType
      frameCount = getAnimationFrameCount animType
      fps = getAnimationFPS animType
      frameNum = getAnimationFrame currentTime frameCount fps
      animName = case animType of
                   AnimIdle -> "idle"
                   AnimAttack -> "attack"
                   AnimDeath -> "death"
                   AnimMove -> "move"
      spritePath = Assets.getAnimatedSpritePath "enemies" spriteName animName frameNum
      cacheKey = "enemy_" ++ spriteName ++ "_" ++ animName ++ "_" ++ show frameNum
  in getSpriteFrame cacheKey spritePath fallback

-- Render animated sprite for tower
renderAnimatedTowerSprite :: TowerType -> Float -> Float -> Picture -> Picture
renderAnimatedTowerSprite towerType lastFireTime currentTime fallback =
  let animType = towerStateToAnimation lastFireTime currentTime
      spriteName = towerTypeToSpriteName towerType
      frameCount = getAnimationFrameCount animType
      fps = getAnimationFPS animType
      frameNum = getAnimationFrame currentTime frameCount fps
      animName = case animType of
                   AnimIdle -> "idle"
                   AnimAttack -> "attack"
                   AnimDeath -> "death"
                   AnimMove -> "idle"  -- Towers don't move
      spritePath = Assets.getAnimatedSpritePath "towers" spriteName animName frameNum
      cacheKey = "tower_" ++ spriteName ++ "_" ++ animName ++ "_" ++ show frameNum
  in getSpriteFrame cacheKey spritePath fallback

-- Render animated sprite for trap
renderAnimatedTrapSprite :: TrapType -> Bool -> Float -> Picture -> Picture
renderAnimatedTrapSprite trapType triggered currentTime fallback =
  let animType = if triggered then AnimAttack else AnimIdle
      spriteName = trapTypeToSpriteName trapType
      frameCount = getAnimationFrameCount animType
      fps = getAnimationFPS animType
      frameNum = getAnimationFrame currentTime frameCount fps
      animName = case animType of
                   AnimIdle -> "idle"
                   AnimAttack -> "attack"
                   AnimDeath -> "death"
                   AnimMove -> "idle"  -- Traps don't move
      spritePath = Assets.getAnimatedSpritePath "traps" spriteName animName frameNum
      cacheKey = "trap_" ++ spriteName ++ "_" ++ animName ++ "_" ++ show frameNum
  in getSpriteFrame cacheKey spritePath fallback

-- Render animated projectile
renderAnimatedProjectileSprite :: ProjectileType -> Float -> Picture -> Picture
renderAnimatedProjectileSprite projType currentTime fallback =
  let spriteName = case projType of
                     Arrow -> "arrow"
                     BallistaBolt -> "ballista_bolt"
                     Fireball -> "fireball"
                     IceShard -> "ice_shard"
                     LightningBolt -> "lightning_bolt"
                     CatapultRock -> "catapult_rock"
                     BarrageShot -> "barrage"
      frameCount = 2  -- Projectiles typically have 2-3 frames
      fps = 10.0
      frameNum = getAnimationFrame currentTime frameCount fps
      spritePath = Assets.getAnimatedSpritePath "projectiles" spriteName "move" frameNum
      cacheKey = "proj_" ++ spriteName ++ "_" ++ show frameNum
  in getSpriteFrame cacheKey spritePath fallback

