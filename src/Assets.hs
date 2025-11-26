{-# LANGUAGE FlexibleContexts #-}

module Assets (ensureAssets, assetPath, loadAllSprites, getSprite, Assets(..),
               getTowerSpritePath, getEnemySpritePath, getTrapSpritePath, getProjectileSpritePath) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)
import Types (TowerType(..), TrapType(..), UnitType(..), ProjectileType(..), AnimationType(..))
import Data.Char (toLower)

assetPath :: FilePath
assetPath = "assets"

imagesPath :: FilePath
imagesPath = assetPath </> "images"

-- Asset cache stored in IORef
{-# NOINLINE assetCache #-}
assetCache :: IORef (Map String Picture)
assetCache = unsafePerformIO $ newIORef M.empty

-- Assets data type to store loaded sprites
data Assets = Assets
  { assetsCache :: Map String Picture
  } deriving (Show)

-- Ensure assets directory exists
ensureAssets :: IO ()
ensureAssets = do
  createDirectoryIfMissing True assetPath
  createDirectoryIfMissing True imagesPath

-- Convert CamelCase to snake_case
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (c:cs) = toLower c : go cs
  where
    go [] = []
    go (c':cs') | c' >= 'A' && c' <= 'Z' = '_' : toLower c' : go cs'
                | otherwise = c' : go cs'

-- Map tower types to sprite names
towerSpriteName :: TowerType -> String
towerSpriteName ArrowTower = "arrow_tower"
towerSpriteName CatapultTower = "catapult_tower"
towerSpriteName CrossbowTower = "crossbow_tower"
towerSpriteName FireTower = "fire_tower"
towerSpriteName TeslaTower = "tesla_tower"
towerSpriteName BallistaTower = "ballista_tower"
towerSpriteName PoisonTower = "poison_tower"
towerSpriteName BombardTower = "bombard_tower"

-- Map enemy types to sprite names
enemySpriteName :: UnitType -> String
enemySpriteName GruntRaider = "grunt_raider"
enemySpriteName BruteCrusher = "brute_crusher"
enemySpriteName Direwolf = "direwolf"
enemySpriteName Shieldbearer = "shieldbearer"
enemySpriteName Pyromancer = "pyromancer"
enemySpriteName Necromancer = "necromancer"
enemySpriteName BoulderRamCrew = "boulder_ram_crew"
enemySpriteName IronbackMinotaur = "ironback_minotaur"
enemySpriteName FireDrake = "fire_drake"
enemySpriteName LichKingArcthros = "lich_king_arcthros"

-- Map trap types to sprite names
trapSpriteName :: TrapType -> String
trapSpriteName SpikeTrap = "spike_trap"
trapSpriteName FreezeTrap = "freeze_trap"
trapSpriteName FirePitTrap = "fire_pit_trap"
trapSpriteName MagicSnareTrap = "magic_snare_trap"
trapSpriteName ExplosiveBarrel = "explosive_barrel"

-- Map projectile types to sprite names
projectileSpriteName :: ProjectileType -> String
projectileSpriteName Arrow = "arrow_projectile"
projectileSpriteName BallistaBolt = "bolt_projectile"
projectileSpriteName Fireball = "fireball_projectile"
projectileSpriteName IceShard = "lightning_arc"  -- Using lightning as placeholder
projectileSpriteName LightningBolt = "lightning_arc"
projectileSpriteName CatapultRock = "rock_projectile"
projectileSpriteName BarrageShot = "cannonball"

-- Map animation types to folder names
animationFolderName :: AnimationType -> String
animationFolderName AnimIdle = "idle"
animationFolderName AnimAttack = "attack"
animationFolderName AnimMove = "move"
animationFolderName AnimDeath = "death"
animationFolderName AnimFlying = "flying"

-- Get sprite path for a specific entity and animation
getSpritePath :: String -> String -> AnimationType -> Int -> FilePath
getSpritePath category name animType frame =
  imagesPath </> category </> name </> animationFolderName animType </> show frame <.> "png"

-- Get sprite path for environment tiles
getEnvironmentTilePath :: String -> FilePath
getEnvironmentTilePath name =
  imagesPath </> "environment" </> name <.> "png"

-- Get sprite path for UI icons
getUIIconPath :: String -> FilePath
getUIIconPath name =
  imagesPath </> "ui" </> name <.> "png"

-- Load a single sprite image
loadSprite :: FilePath -> IO (Maybe Picture)
loadSprite path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- loadJuicyPNG path
      case result of
        Just pic -> return $ Just pic
        Nothing -> return Nothing
    else return Nothing

-- Load all sprites into cache
loadAllSprites :: IO Assets
loadAllSprites = do
  ensureAssets
  cache <- newIORef M.empty
  
  -- Load tower sprites
  let towerTypes = [ArrowTower, CatapultTower, CrossbowTower, FireTower, TeslaTower, BallistaTower, PoisonTower, BombardTower]
      animTypes = [AnimIdle, AnimAttack, AnimDeath]
  
  forM_ towerTypes $ \towerType -> do
    let name = towerSpriteName towerType
    forM_ animTypes $ \animType -> do
      let frameCount = case animType of
            AnimIdle -> 3
            AnimAttack -> 5
            AnimDeath -> 4
            _ -> 1
      forM_ [0..frameCount-1] $ \frame -> do
        let path = getSpritePath "towers" name animType frame
        sprite <- loadSprite path
        case sprite of
          Just pic -> modifyIORef cache (M.insert (path) pic)
          Nothing -> return ()
  
  -- Load enemy sprites
  let enemyTypes = [GruntRaider, BruteCrusher, Direwolf, Shieldbearer, Pyromancer, Necromancer, BoulderRamCrew]
      enemyAnimTypes = [AnimIdle, AnimMove, AnimAttack, AnimDeath]
  
  forM_ enemyTypes $ \enemyType -> do
    let name = enemySpriteName enemyType
    forM_ enemyAnimTypes $ \animType -> do
      let frameCount = case animType of
            AnimIdle -> 3
            AnimMove -> 6
            AnimAttack -> 5
            AnimDeath -> 5
            _ -> 1
      forM_ [0..frameCount-1] $ \frame -> do
        let path = getSpritePath "enemies" name animType frame
        sprite <- loadSprite path
        case sprite of
          Just pic -> modifyIORef cache (M.insert path pic)
          Nothing -> return ()
  
  -- Load boss sprites
  let bossTypes = [IronbackMinotaur, FireDrake, LichKingArcthros]
  
  forM_ bossTypes $ \bossType -> do
    let name = enemySpriteName bossType
    forM_ enemyAnimTypes $ \animType -> do
      let frameCount = case animType of
            AnimIdle -> 3
            AnimMove -> 6
            AnimAttack -> 6
            AnimDeath -> 6
            _ -> 1
      forM_ [0..frameCount-1] $ \frame -> do
        let path = getSpritePath "bosses" name animType frame
        sprite <- loadSprite path
        case sprite of
          Just pic -> modifyIORef cache (M.insert path pic)
          Nothing -> return ()
  
  -- Load trap sprites
  let trapTypes = [SpikeTrap, FreezeTrap, FirePitTrap, MagicSnareTrap, ExplosiveBarrel]
      trapAnimTypes = [AnimIdle, AnimAttack]  -- idle and trigger
  
  forM_ trapTypes $ \trapType -> do
    let name = trapSpriteName trapType
    forM_ trapAnimTypes $ \animType -> do
      let frameCount = case animType of
            AnimIdle -> 2
            AnimAttack -> 4
            _ -> 1
      forM_ [0..frameCount-1] $ \frame -> do
        let path = getSpritePath "traps" name animType frame
        sprite <- loadSprite path
        case sprite of
          Just pic -> modifyIORef cache (M.insert path pic)
          Nothing -> return ()
  
  -- Load projectile sprites
  let projTypes = [Arrow, BallistaBolt, Fireball, IceShard, LightningBolt, CatapultRock, BarrageShot]
  
  forM_ projTypes $ \projType -> do
    let name = projectileSpriteName projType
    forM_ [0..2] $ \frame -> do  -- 3 frames for projectiles
      let path = getSpritePath "projectiles" name AnimFlying frame
      sprite <- loadSprite path
      case sprite of
        Just pic -> modifyIORef cache (M.insert path pic)
        Nothing -> return ()
  
  -- Load environment tiles
  let envTiles = ["grass_tile_A", "grass_tile_B", "grass_tile_C",
                  "path_tile_A", "path_tile_B", "path_tile_C",
                  "castle_wall_straight_horizontal", "castle_wall_straight_vertical",
                  "castle_wall_corner_NE", "castle_wall_corner_NW",
                  "castle_wall_corner_SE", "castle_wall_corner_SW",
                  "castle_gate_closed", "castle_gate_open_top", "castle_gate_open_bottom",
                  "castle_floor_stone"]
  
  forM_ envTiles $ \tile -> do
    let path = getEnvironmentTilePath tile
    sprite <- loadSprite path
    case sprite of
      Just pic -> modifyIORef cache (M.insert path pic)
      Nothing -> return ()
  
  -- Load UI icons
  let uiIcons = ["ui_coin", "ui_heart", "ui_shield", "ui_sword", "ui_fire", "ui_lightning", "ui_poison", "ui_skull"]
  
  forM_ uiIcons $ \icon -> do
    let path = getUIIconPath icon
    sprite <- loadSprite path
    case sprite of
      Just pic -> modifyIORef cache (M.insert path pic)
      Nothing -> return ()
  
  finalCache <- readIORef cache
  return $ Assets { assetsCache = finalCache }

-- Get a sprite from cache by path
getSprite :: FilePath -> Assets -> Maybe Picture
getSprite path assets = M.lookup path (assetsCache assets)

-- Helper to get sprite path for rendering
getTowerSpritePath :: TowerType -> AnimationType -> Int -> FilePath
getTowerSpritePath towerType animType frame =
  getSpritePath "towers" (towerSpriteName towerType) animType frame

getEnemySpritePath :: UnitType -> AnimationType -> Int -> FilePath
getEnemySpritePath enemyType animType frame =
  let category = if enemyType `elem` [IronbackMinotaur, FireDrake, LichKingArcthros]
                 then "bosses"
                 else "enemies"
  in getSpritePath category (enemySpriteName enemyType) animType frame

getTrapSpritePath :: TrapType -> AnimationType -> Int -> FilePath
getTrapSpritePath trapType animType frame =
  getSpritePath "traps" (trapSpriteName trapType) animType frame

getProjectileSpritePath :: ProjectileType -> Int -> FilePath
getProjectileSpritePath projType frame =
  getSpritePath "projectiles" (projectileSpriteName projType) AnimFlying frame
