{-# LANGUAGE FlexibleContexts #-}

module Rendering.SpriteAnimation where

import Graphics.Gloss
import Types (AnimationType(..), AnimationState(..), UnitType(..), TowerType(..), TrapType(..), ProjectileType(..), EnemyAIState(..))
import Rendering.PixelArt (enemyStateToAnimation, towerStateToAnimation, pixelColor, drawPixelSprite)
import qualified Rendering.PixelArt
import Assets (getTowerSpritePath, getEnemySpritePath, getTrapSpritePath, getProjectileSpritePath, getSprite, Assets(..))
import Constants (enemyBaseSize, towerBaseSize, projectileBaseSize)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- ============================================================================
-- Global Assets Cache
-- ============================================================================

{-# NOINLINE globalAssets #-}
globalAssets :: IORef (Maybe Assets)
globalAssets = unsafePerformIO $ newIORef Nothing

-- Get or load sprite
getSpriteFromCache :: FilePath -> Maybe Picture
getSpriteFromCache path = unsafePerformIO $ do
  assetsRef <- readIORef globalAssets
  case assetsRef of
    Just assets -> return $ getSprite path assets
    Nothing -> do
      -- Try to load directly
      result <- loadJuicyPNG path
      return result

-- ============================================================================
-- Animation Frame Configuration
-- ============================================================================

-- Frame counts for each animation type
animFrameCounts :: AnimationType -> Int
animFrameCounts AnimIdle = 3      -- 2-3 frames
animFrameCounts AnimAttack = 5    -- 4-6 frames
animFrameCounts AnimMove = 6       -- 4-6 frames (enemies only)
animFrameCounts AnimDeath = 5     -- 4-6 frames
animFrameCounts AnimFlying = 3     -- 2-3 frames (projectiles)

-- Animation frame rate (frames per second)
animFPS :: Float
animFPS = 8.0  -- 8 FPS for smooth pixel art animation

-- Time per frame
frameTime :: Float
frameTime = 1.0 / animFPS

-- ============================================================================
-- Animation State Updates
-- ============================================================================

-- Update animation state based on time
updateAnimationState :: Float -> AnimationState -> AnimationState
updateAnimationState dt animState =
  let newTime = animTime animState + dt
      frameCount = animFrameCounts (animType animState)
      -- Advance frame if enough time has passed
      (newFrame, finalTime) = if newTime >= frameTime
                              then ((animFrame animState + 1) `mod` frameCount, newTime - frameTime)
                              else (animFrame animState, newTime)
  in animState { animFrame = newFrame, animTime = finalTime }

-- Update enemy animation based on AI state
updateEnemyAnimation :: Float -> EnemyAIState -> AnimationState -> AnimationState
updateEnemyAnimation dt aiState animState =
  let newAnimType = enemyStateToAnimation aiState
      -- Reset frame if animation type changed
      updatedState = if animType animState /= newAnimType
                     then animState { animType = newAnimType, animFrame = 0, animTime = 0 }
                     else animState
  in updateAnimationState dt updatedState

-- Update tower animation based on firing state
updateTowerAnimation :: Float -> Float -> Float -> AnimationState -> AnimationState
updateTowerAnimation dt lastFireTime currentTime animState =
  let newAnimType = towerStateToAnimation lastFireTime currentTime
      -- Reset frame if animation type changed
      updatedState = if animType animState /= newAnimType
                     then animState { animType = newAnimType, animFrame = 0, animTime = 0 }
                     else animState
  in updateAnimationState dt updatedState

-- Update trap animation based on trigger state
updateTrapAnimation :: Float -> Bool -> AnimationState -> AnimationState
updateTrapAnimation dt triggered animState =
  let newAnimType = if triggered then AnimAttack else AnimIdle
      -- Reset frame if animation type changed
      updatedState = if animType animState /= newAnimType
                     then animState { animType = newAnimType, animFrame = 0, animTime = 0 }
                     else animState
  in updateAnimationState dt updatedState

-- ============================================================================
-- Sprite Rendering with Image Loading
-- ============================================================================

-- Render enemy with animation frame using loaded sprite
renderAnimatedEnemy :: UnitType -> AnimationState -> Float -> Picture
renderAnimatedEnemy unitType animState size =
  let animType' = animType animState
      frame = animFrame animState
      spritePath = getEnemySpritePath unitType animType' frame
      sprite = getSpriteFromCache spritePath
  in case sprite of
    Just pic -> scale (size / enemyBaseSize) (size / enemyBaseSize) pic  -- Scale from 48x48 base
    Nothing -> blank  -- Fallback to blank if sprite not found

-- Render tower with animation frame using loaded sprite
renderAnimatedTower :: TowerType -> AnimationState -> Float -> Picture
renderAnimatedTower towerType animState size =
  let animType' = animType animState
      frame = animFrame animState
      spritePath = getTowerSpritePath towerType animType' frame
      sprite = getSpriteFromCache spritePath
  in case sprite of
    Just pic -> scale (size / towerBaseSize) (size / towerBaseSize) pic  -- Scale from 48x48 base
    Nothing -> blank

-- Render trap with animation frame using loaded sprite
renderAnimatedTrap :: TrapType -> AnimationState -> Float -> Picture
renderAnimatedTrap trapType animState size =
  let animType' = animType animState
      frame = animFrame animState
      spritePath = getTrapSpritePath trapType animType' frame
      sprite = getSpriteFromCache spritePath
  in case sprite of
    Just pic -> scale (size / 32.0) (size / 32.0) pic
    Nothing -> blank

-- Render projectile with animation frame using loaded sprite
renderAnimatedProjectile :: ProjectileType -> AnimationState -> Float -> Picture
renderAnimatedProjectile projType animState size =
  let frame = animFrame animState
      spritePath = getProjectileSpritePath projType frame
      sprite = getSpriteFromCache spritePath
  in case sprite of
    Just pic -> scale (size / projectileBaseSize) (size / projectileBaseSize) pic  -- Scale from 24x24 base
    Nothing -> blank

-- ============================================================================
-- Environment Tile Rendering with Image Loading
-- ============================================================================

-- Render grass tile using loaded sprite with decorative elements
renderGrassTile :: Float -> Float -> Picture
renderGrassTile x y =
  let tilePath = "assets/images/environment/grass_tile_A.png"
      sprite = getSpriteFromCache tilePath
      baseTile = case sprite of
        Just pic -> translate x y $ scale 2.0 2.0 pic  -- Scale 32x32 to 64x64
        Nothing -> blank
      -- Add random decorative elements (small flowers, grass tufts, etc.)
      -- Use tile position as seed for pseudo-random decoration
      seed = floor (x * 100 + y * 100) `mod` 100
      decorations = if seed < 15  -- 15% chance of decoration
        then renderGrassDecoration x y seed
        else blank
  in pictures [baseTile, decorations]

-- Render small decorative elements on grass tiles
renderGrassDecoration :: Float -> Float -> Int -> Picture
renderGrassDecoration x y seed =
  let decoType = seed `mod` 4
      pixelSize = 2
      offsetX = fromIntegral ((seed * 7) `mod` 20 - 10)  -- Random offset within tile
      offsetY = fromIntegral ((seed * 11) `mod` 20 - 10)
  in translate (x + offsetX) (y + offsetY) $ case decoType of
    0 -> -- Small yellow flower
      pictures $ map (\(px, py, col) ->
        translate (px * pixelSize) (py * pixelSize) $
        color (pixelColor col) $
        rectangleSolid pixelSize pixelSize
        ) [(-1, 0, "yellow"), (0, -1, "yellow"), (0, 0, "yellow"), (0, 1, "yellow"), (1, 0, "yellow")]
    1 -> -- Small white flower
      pictures $ map (\(px, py, col) ->
        translate (px * pixelSize) (py * pixelSize) $
        color (pixelColor col) $
        rectangleSolid pixelSize pixelSize
        ) [(-1, 0, "white"), (0, -1, "white"), (0, 0, "yellow"), (0, 1, "white"), (1, 0, "white")]
    2 -> -- Grass tuft (dark green)
      pictures $ map (\(px, py, col) ->
        translate (px * pixelSize) (py * pixelSize) $
        color (pixelColor col) $
        rectangleSolid pixelSize pixelSize
        ) [(-1, 0, "dark green"), (0, -1, "dark green"), (0, 0, "green"), (0, 1, "dark green"), (1, 0, "dark green")]
    _ -> -- Small rock
      pictures $ map (\(px, py, col) ->
        translate (px * pixelSize) (py * pixelSize) $
        color (pixelColor col) $
        rectangleSolid pixelSize pixelSize
        ) [(-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray"), (-1, 1, "gray"), (0, 1, "dark gray"), (1, 1, "gray")]
  where
    pixelColor = Rendering.PixelArt.pixelColor

-- Render path tile using loaded sprite
renderPathTile :: Float -> Float -> Picture
renderPathTile x y =
  let tilePath = "assets/images/environment/path_tile_A.png"
      sprite = getSpriteFromCache tilePath
  in case sprite of
    Just pic -> translate x y $ scale 2.0 2.0 pic  -- Scale 32x32 to 64x64
    Nothing -> blank

-- ============================================================================
-- Effect Rendering
-- ============================================================================

-- Render hit spark effect
renderHitSpark :: Float -> Float -> Float -> Picture
renderHitSpark x y life =
  let pixelSize = 4
      alpha = life * 2  -- Fade out
      sparkColor = makeColor 1.0 1.0 0.0 alpha
      pixels = [ (0, 0, "yellow"), (-1, 0, "yellow"), (1, 0, "yellow"), (0, -1, "yellow"), (0, 1, "yellow") ]
  in translate x y $ pictures $ map (\(px, py, _) ->
    translate (px * pixelSize) (py * pixelSize) $
    color sparkColor $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- Render explosion effect
renderExplosion :: Float -> Float -> Float -> Picture
renderExplosion x y life =
  let pixelSize = 6
      alpha = life * 1.5
      -- Explosion with orange/yellow/red
      pixels = [ (-2, 0, "orange"), (-1, 0, "yellow"), (0, 0, "red"), (1, 0, "yellow"), (2, 0, "orange")
               , (0, -2, "orange"), (0, -1, "yellow"), (0, 1, "yellow"), (0, 2, "orange")
               , (-1, -1, "orange"), (1, -1, "orange"), (-1, 1, "orange"), (1, 1, "orange")
               ]
  in translate x y $ pictures $ map (\(px, py, col) ->
    translate (px * pixelSize) (py * pixelSize) $
    color (makeColorAlpha (pixelColor col) alpha) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- Helper to add alpha to color (simplified - just use the color with new alpha)
makeColorAlpha :: Color -> Float -> Color
makeColorAlpha col alpha = col  -- For now, just return the color (alpha handling in Gloss is limited)
