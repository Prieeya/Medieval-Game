module Rendering.RenderWorld where

import Graphics.Gloss
import Types
import Constants
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import qualified Data.IORef as IORef
import System.FilePath ((</>))
import Data.Char (toLower)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import qualified Assets
import Rendering.SpriteAnimation (renderAnimatedEnemySprite, renderAnimatedTowerSprite, renderAnimatedTrapSprite, renderAnimatedProjectileSprite, getSpriteFrame)

-- Legacy sprite loader (for backward compatibility with old static sprites)
getSprite :: String -> FilePath -> Picture -> Picture
getSprite key fp fallback = getSpriteFrame key fp fallback

-- ============================================================================
-- Realistic Color Definitions (Medieval Themed)
-- ============================================================================

brown :: Color
brown = makeColor 0.55 0.35 0.15 1

darkBrown :: Color
darkBrown = makeColor 0.35 0.2 0.1 1

stoneGray :: Color
stoneGray = makeColor 0.5 0.5 0.5 1

darkStoneGray :: Color
darkStoneGray = makeColor 0.3 0.3 0.3 1

gold :: Color
gold = makeColor 1.0 0.84 0.0 1

darkGold :: Color
darkGold = makeColor 0.7 0.6 0.0 1

ironGray :: Color
ironGray = makeColor 0.35 0.35 0.4 1

darkIron :: Color
darkIron = makeColor 0.2 0.2 0.25 1

woodBrown :: Color
woodBrown = makeColor 0.45 0.3 0.15 1

-- ============================================================================
-- Main Render Function
-- ============================================================================

renderWorld :: World -> Picture
renderWorld world = pictures
  [ renderBackground
  , renderPaths world
  , renderFort world
  , renderCastle world
  , renderDeploymentPreview world
  , renderTraps world
  , renderEnemies world
  , renderTowers world
  , renderProjectiles world
  , renderVisualEffects world
  ]

-- ============================================================================
-- Background
-- ============================================================================

renderBackground :: Picture
renderBackground = 
  -- Enhanced medieval battlefield background with better visual appeal
  let -- Base ground with gradient effect
      groundColor = makeColor 0.4 0.35 0.28 1  -- Warmer earth tone
      darkerGround = makeColor 0.32 0.28 0.22 1
      -- Strategic grass patches for visual interest
      grassPatches = pictures $ map (\i -> 
        let x = worldLeft + fromIntegral (i `mod` 18) * (worldWidth / 18)
            y = worldBottom + fromIntegral (i `div` 18) * (worldHeight / 14)
            grassColor = makeColor 0.28 0.42 0.24 0.7
            size = 25 + fromIntegral (i `mod` 3) * 5
        in translate x y $ color grassColor $ circleSolid size
        ) [0..251]
      -- Dirt patches for texture
      dirtPatches = pictures $ map (\i ->
        let x = worldLeft + fromIntegral (i `mod` 22) * (worldWidth / 22)
            y = worldBottom + fromIntegral (i `div` 22) * (worldHeight / 16)
            dirtColor = makeColor 0.35 0.28 0.22 0.5
            size = 18 + fromIntegral (i `mod` 2) * 4
        in translate x y $ color dirtColor $ circleSolid size
        ) [0..351]
      -- Stone/rock details
      stoneDetails = pictures $ map (\i ->
        let x = worldLeft + fromIntegral (i `mod` 30) * (worldWidth / 30)
            y = worldBottom + fromIntegral (i `div` 30) * (worldHeight / 20)
            stoneColor = makeColor 0.45 0.42 0.38 0.6
        in translate x y $ color stoneColor $ circleSolid 8
        ) [0..599]
  in pictures
    [ color groundColor $ rectangleSolid worldWidth worldHeight
    , color darkerGround $ translate 0 (-worldHeight/4) $ rectangleSolid worldWidth (worldHeight/2)
    , grassPatches
    , dirtPatches
    , stoneDetails
    ]

-- ============================================================================
-- Paths (Debug)
-- ============================================================================

renderPaths :: World -> Picture
renderPaths world = pictures []

-- ============================================================================
-- Fort Rendering
-- ============================================================================

renderFort :: World -> Picture
renderFort world = pictures
  [ renderWalls (fortWalls $ fort world)
  , renderGate (fortGate $ fort world)
  ]

renderWalls :: [WallSegment] -> Picture
renderWalls walls = pictures $ map renderWall walls

renderWall :: WallSegment -> Picture
renderWall wall =
  let (x1, y1) = wallStart wall
      (x2, y2) = wallEnd wall
      healthRatio = wallHP wall / (Types.wallMaxHP wall)
      wallColor = if healthRatio > 0.7 then darkBrown
                  else if healthRatio > 0.4 then (makeColor 0.4 0.25 0.15 1)
                  else (makeColor 0.2 0.15 0.08 1)
      -- Make walls thick and realistic
      wallThickness = 16
      -- Calculate perpendicular direction for thickness
      dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
      perpX = if len > 0 then -dy / len * wallThickness / 2 else 0
      perpY = if len > 0 then dx / len * wallThickness / 2 else 0
      -- Create thick wall rectangle
      corners = [(x1 + perpX, y1 + perpY), (x2 + perpX, y2 + perpY),
                (x2 - perpX, y2 - perpY), (x1 - perpX, y1 - perpY)]
      wallBody = polygon corners
      -- Add darker outline for depth
      outlineColor = makeColor 0.2 0.15 0.1 1
      outline = line corners
  in pictures [color wallColor wallBody, color outlineColor outline]

renderGate :: Gate -> Picture
renderGate gate =
  let (x, y) = gatePos gate
      healthRatio = gateHP gate / (Types.gateMaxHP gate)
      gateHeight = Types.gateWidth gate
  in translate x y $ pictures
    [ if gateDestroyed gate
      then pictures
        [ color darkStoneGray $ rectangleSolid 20 gateHeight
        , color (makeColor 0.2 0.1 0.05 1) $ rectangleSolid 16 (gateHeight * 0.7)
        ]
      else pictures
        [ color darkBrown $ rectangleSolid 18 gateHeight
        , color woodBrown $ rectangleSolid 14 (gateHeight * 0.9)
        , color stoneGray $ rectangleSolid 10 (gateHeight * 0.8)
        , translate (-6) (gateHeight * 0.3) $ color darkIron $ rectangleSolid 4 (gateHeight * 0.5)
        , translate 6 (gateHeight * 0.3) $ color darkIron $ rectangleSolid 4 (gateHeight * 0.5)
        , translate 0 (gateHeight/2 + 12) $ renderHealthBar (gateHP gate) (Types.gateMaxHP gate) 35
        ]
    ]

-- ============================================================================
-- Castle Rendering
-- ============================================================================

renderCastle :: World -> Picture
renderCastle world =
  let c = castle world
      (x, y) = castlePos c
      size = Types.castleSize c
  in translate x y $ pictures
    [ color darkBrown $ rectangleSolid (size + 10) (size + 10)
    , color woodBrown $ rectangleSolid size size
    , color (makeColor 0.7 0.5 0.3 1) $ rectangleSolid (size * 0.8) (size * 0.8)
    , color darkStoneGray $ translate (-size * 0.3) (size * 0.4) $ rectangleSolid (size * 0.3) (size * 0.4)
    , color darkStoneGray $ translate (size * 0.3) (size * 0.4) $ rectangleSolid (size * 0.3) (size * 0.4)
    , color darkGold $ translate 0 (size * 0.35) $ rectangleSolid (size * 0.6) (size * 0.5)
    , translate 0 (size/2 + 15) $ renderHealthBar (castleHP c) (Types.castleMaxHP c) (size * 0.7)
    ]

-- ============================================================================
-- Enemy Rendering
-- ============================================================================

renderEnemies :: World -> Picture
renderEnemies world = pictures $ map (renderEnemy (timeElapsed world)) $ M.elems (enemies world)

renderEnemy :: Float -> Enemy -> Picture
renderEnemy currentTime enemy =
  let (x, y) = enemyPos enemy
      baseColor = enemyTypeColor (enemyType enemy)
      flashColor = if enemyHitFlash enemy > 0
                   then makeColor 1 1 1 (enemyHitFlash enemy * 5)
                   else makeColor 0 0 0 0
      size = enemySizeByRole (enemyRole enemy)
      healthBar = renderHealthBar (enemyHP enemy) (enemyMaxHP enemy) size
      sprite = renderEnemySprite (enemyType enemy) (enemyAIState enemy) currentTime baseColor size
  in translate x y $ pictures
    [ sprite
    , color flashColor $ circleSolid (size + 2)
    , translate 0 (size + 8) healthBar
    ]

-- Fallback shape-based enemy sprites (kept as-is for when PNGs are missing)
renderEnemySpriteShape :: UnitType -> Color -> Float -> Picture
-- Normal Enemies
renderEnemySpriteShape GruntRaider _ size =
  pictures
    [ color (makeColor 0.5 0.3 0.2 1) $ rectangleSolid (size * 1.2) (size * 1.2)
    , color (makeColor 0.6 0.35 0.25 1) $ rectangleSolid (size * 0.9) (size * 0.9)
    , color darkIron $ translate 0 (size * 0.4) $ rectangleSolid (size * 0.3) (size * 0.3)
    ]
renderEnemySpriteShape BruteCrusher _ size =
  pictures
    [ color (makeColor 0.45 0.25 0.15 1) $ rectangleSolid (size * 2.2) (size * 2)
    , color (makeColor 0.6 0.35 0.2 1) $ rectangleSolid (size * 1.8) (size * 1.6)
    , color darkStoneGray $ translate 0 (size * 0.4) $ rectangleSolid (size * 0.8) (size * 0.8)
    ]
renderEnemySpriteShape Direwolf _ size =
  pictures
    [ color (makeColor 0.2 0.2 0.2 1) $ rectangleSolid (size * 1.1) (size * 0.8)
    , color (makeColor 0.3 0.3 0.3 1) $ rectangleSolid (size * 0.8) (size * 0.6)
    , color (makeColor 0.4 0.4 0.4 1) $ translate (size * 0.3) 0 $ circleSolid (size * 0.2)
    ]
renderEnemySpriteShape Shieldbearer _ size =
  pictures
    [ color stoneGray $ rectangleSolid (size * 1.6) (size * 1.6)
    , color darkStoneGray $ rectangleSolid (size * 1.3) (size * 1.3)
    , color ironGray $ circleSolid (size * 0.9)
    , color darkIron $ translate 0 (size * 0.5) $ rectangleSolid (size * 0.4) (size * 0.3)
    ]
renderEnemySpriteShape Pyromancer _ size =
  pictures
    [ color (makeColor 0.3 0.2 0.2 1) $ rectangleSolid (size * 1.3) (size * 1.5)
    , color (makeColor 0.5 0.3 0.2 1) $ rectangleSolid (size * 1.0) (size * 1.2)
    , color (makeColor 0.8 0.4 0.1 1) $ translate 0 (size * 0.5) $ circleSolid (size * 0.3)
    ]
renderEnemySpriteShape Necromancer _ size =
  pictures
    [ color (makeColor 0.2 0.2 0.25 1) $ rectangleSolid (size * 1.4) (size * 1.6)
    , color (makeColor 0.3 0.3 0.35 1) $ rectangleSolid (size * 1.1) (size * 1.3)
    , color (makeColor 0.5 0.5 0.6 1) $ translate 0 (size * 0.6) $ circleSolid (size * 0.25)
    ]
renderEnemySpriteShape BoulderRamCrew _ size =
  pictures
    [ color darkBrown $ rectangleSolid (size * 2.2) (size * 1.8)
    , color woodBrown $ rectangleSolid (size * 2.0) (size * 1.6)
    , color ironGray $ rectangleSolid (size * 1.4) (size * 2.4)
    , color darkIron $ translate 0 (size * 1.0) $ rectangleSolid (size * 0.6) (size * 0.4)
    ]
-- Bosses
renderEnemySpriteShape IronbackMinotaur _ size =
  pictures
    [ color darkBrown $ rectangleSolid (size * 2.6) (size * 2.6)
    , color (makeColor 0.6 0.4 0.2 1) $ rectangleSolid (size * 2.2) (size * 2.2)
    , color ironGray $ translate 0 (size * 0.8) $ rectangleSolid (size * 0.8) (size * 0.6)
    , color darkIron $ translate (-size * 0.4) (size * 1.0) $ rectangleSolid (size * 0.3) (size * 0.4)
    , color darkIron $ translate (size * 0.4) (size * 1.0) $ rectangleSolid (size * 0.3) (size * 0.4)
    ]
renderEnemySpriteShape FireDrake _ size =
  pictures
    [ color (makeColor 0.7 0.3 0.1 1) $ rectangleSolid (size * 2.4) (size * 2.2)
    , color (makeColor 0.8 0.4 0.2 1) $ rectangleSolid (size * 2.0) (size * 1.8)
    , color (makeColor 1 0.5 0.2 1) $ translate 0 (size * 0.6) $ circleSolid (size * 0.4)
    , color (makeColor 0.9 0.3 0.1 1) $ translate 0 (size * 1.0) $ rectangleSolid (size * 0.6) (size * 0.4)
    ]
renderEnemySpriteShape LichKingArcthros _ size =
  pictures
    [ color (makeColor 0.2 0.2 0.3 1) $ rectangleSolid (size * 2.2) (size * 2.6)
    , color (makeColor 0.3 0.3 0.4 1) $ rectangleSolid (size * 1.8) (size * 2.2)
    , color (makeColor 0.4 0.5 0.6 1) $ translate 0 (size * 1.1) $ circleSolid (size * 0.3)
    , color (makeColor 0.3 0.6 0.8 1) $ translate 0 (size * 0.3) $ circleSolid (size * 0.25)
    ]

-- Generic sprite wrapper: attempt to load animated PNG asset for unit type, fallback to shape sprite
renderEnemySprite :: UnitType -> EnemyAIState -> Float -> Color -> Float -> Picture
renderEnemySprite ut state currentTime col size =
  let fallback = renderEnemySpriteShape ut col size
  in renderAnimatedEnemySprite ut state currentTime fallback

enemyTypeColor :: UnitType -> Color
enemyTypeColor GruntRaider = makeColor 0.6 0.2 0.2 1
enemyTypeColor BruteCrusher = makeColor 0.6 0.4 0.3 1
enemyTypeColor Direwolf = makeColor 0.2 0.2 0.2 1
enemyTypeColor Shieldbearer = makeColor 0.5 0.5 0.6 1
enemyTypeColor Pyromancer = makeColor 0.8 0.3 0.1 1
enemyTypeColor Necromancer = makeColor 0.3 0.3 0.4 1
enemyTypeColor BoulderRamCrew = makeColor 0.4 0.3 0.3 1
enemyTypeColor IronbackMinotaur = makeColor 0.7 0.4 0.2 1
enemyTypeColor FireDrake = makeColor 0.8 0.2 0.1 1
enemyTypeColor LichKingArcthros = makeColor 0.2 0.3 0.5 1

enemySizeByRole :: UnitRole -> Float
enemySizeByRole Melee = 8
enemySizeByRole Fast = 6
enemySizeByRole Ranged = 7
enemySizeByRole Heavy = 12
enemySizeByRole Siege = 10
enemySizeByRole Boss = 16

-- ============================================================================
-- Tower Rendering
-- ============================================================================

renderTowers :: World -> Picture
renderTowers world = pictures $ map (renderTower (timeElapsed world)) $ M.elems (towers world)

renderTower :: Float -> Tower -> Picture
renderTower currentTime tower =
  let (x, y) = towerPos tower
      towerColor = towerTypeColor (towerType tower)
      size = 12 + fromIntegral (towerLevel tower) * 3
      range = towerRange tower
      -- Arch-shaped range indicator based on tower role
      rangeArch = renderTowerRangeArch (towerType tower) range
      healthBar = renderHealthBar (towerHP tower) (Types.towerMaxHP tower) size
      -- Use animated sprite system
      fallback = renderTowerSprite (towerType tower) size (towerLevel tower)
      sprite = renderAnimatedTowerSprite (towerType tower) (towerLastFireTime tower) currentTime fallback
  in translate x y $ pictures
    [ rangeArch
    , sprite
    , translate 0 (size + 8) healthBar
    , color white $ translate 0 (size/2 + 8) $ scale 0.12 0.12 $ text $ "Lvl" ++ show (towerLevel tower)
    ]

-- Render arch-shaped range indicator based on tower type
renderTowerRangeArch :: TowerType -> Float -> Picture
renderTowerRangeArch tt range =
  let -- More visible arch with better colors
      archColor = makeColor 0.2 0.8 0.2 0.4  -- Green for valid placement
      archOutline = makeColor 0.1 0.6 0.1 0.6  -- Darker outline
      -- Different arch angles based on tower role
      (angle, segments) = case tt of
        ArrowTower -> (120, 30)  -- Wide arch for archers
        CatapultTower -> (100, 25)  -- Medium arch for catapult
        CrossbowTower -> (90, 20)  -- Narrow arch for sniper
        FireTower -> (150, 40)  -- Very wide for fire
        TeslaTower -> (140, 35)  -- Wide for chain lightning
        BallistaTower -> (110, 30)  -- Medium-wide for piercing
        PoisonTower -> (130, 35)  -- Wide for debuff
        BombardTower -> (120, 30)  -- Wide for cannon
      -- Rotate arch 180 degrees to point left (toward enemies)
      -- Start from 180 - angle/2 to point left, then sweep the angle
      startAngle = 180 - angle / 2
      angleStep = angle / fromIntegral segments
      points = map (\i -> 
        let a = startAngle + angleStep * fromIntegral i
            rad = a * pi / 180
        in (range * cos rad, range * sin rad)
        ) [0..segments]
      archLine = line points
      -- Add lines from center to arch ends for better visibility
      endPoint1 = head points
      endPoint2 = last points
      centerLines = pictures
        [ line [(0, 0), endPoint1]
        , line [(0, 0), endPoint2]
        ]
  in pictures
    [ color archColor archLine
    , color archOutline archLine  -- Thicker outline
    , color archColor centerLines
    ]

-- Render deployment preview with arch-shaped radius
renderDeploymentPreview :: World -> Picture
renderDeploymentPreview world =
  case buildMode (inputState world) of
    PlaceTower tt ->
      let (mx, my) = mouseWorldPos (inputState world)
          (range, _, _) = towerStats tt
          -- Check if position is valid (simplified check)
          isInside = mx >= fortLeft && mx <= fortRight && my >= fortBottom && my <= fortTop
          tooCloseToTower = any (\t -> distance (towerPos t) (mx, my) < 50) (M.elems $ towers world)
          tooCloseToGate = distance (mx, my) (gatePos $ fortGate $ fort world) < 60
          tooCloseToCastle = distance (mx, my) (castlePos $ castle world) < 100
          isValid = isInside && not tooCloseToTower && not tooCloseToGate && not tooCloseToCastle
          -- Use different colors for arch based on validity
          (archColor, archOutline) = if isValid
                        then (makeColor 0.2 1.0 0.2 0.5, makeColor 0.1 0.7 0.1 0.7)
                        else (makeColor 1.0 0.2 0.2 0.5, makeColor 0.7 0.1 0.1 0.7)
          -- Render arch with proper colors
          (angle, segments) = case tt of
            ArrowTower -> (120, 30)
            CatapultTower -> (100, 25)
            CrossbowTower -> (90, 20)
            FireTower -> (150, 40)
            TeslaTower -> (140, 35)
            BallistaTower -> (110, 30)
            PoisonTower -> (130, 35)
            BombardTower -> (120, 30)
          -- Rotate arch 180 degrees to point left (toward enemies)
          startAngle = 180 - angle / 2
          angleStep = angle / fromIntegral segments
          points = map (\i -> 
            let a = startAngle + angleStep * fromIntegral i
                rad = a * pi / 180
            in (range * cos rad, range * sin rad)
            ) [0..segments]
          archLine = line points
          endPoint1 = head points
          endPoint2 = last points
          centerLines = pictures
            [ line [(0, 0), endPoint1]
            , line [(0, 0), endPoint2]
            ]
          rangeArch = pictures
            [ color archColor archLine
            , color archOutline archLine
            , color archColor centerLines
            ]
          previewTower = color (if isValid then makeColor 0.2 1.0 0.2 0.6 else makeColor 1.0 0.2 0.2 0.6) $ circleSolid 15
      in translate mx my $ pictures [rangeArch, previewTower]
    _ -> blank
  where
    distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

renderTowerSprite :: TowerType -> Float -> Int -> Picture
renderTowerSprite ArrowTower size lvl =
  pictures
    [ color woodBrown $ rectangleSolid (size * 1.3) (size * 1.3)
    , color darkBrown $ rectangleSolid (size * 1.1) (size * 1.1)
    , color (makeColor 0.5 0.6 0.4 1) $ rectangleSolid (size * 0.8) (size * 0.8)
    , color darkGold $ line [(-size * 0.35, 0), (size * 0.35, 0)]
    ]
renderTowerSprite CatapultTower size lvl =
  pictures
    [ color stoneGray $ rectangleSolid (size * 1.5) (size * 1.5)
    , color darkStoneGray $ rectangleSolid (size * 1.3) (size * 1.3)
    , color woodBrown $ rectangleSolid (size * 1.0) (size * 1.2)
    , color darkBrown $ translate (size * 0.3) 0 $ rectangleSolid (size * 0.4) (size * 0.3)
    ]
renderTowerSprite CrossbowTower size lvl =
  pictures
    [ color woodBrown $ rectangleSolid (size * 1.4) (size * 1.4)
    , color darkBrown $ rectangleSolid (size * 1.2) (size * 1.2)
    , color ironGray $ rectangleSolid (size * 0.9) (size * 1.3)
    , color darkIron $ translate 0 (size * 0.3) $ rectangleSolid (size * 0.5) (size * 0.3)
    ]
renderTowerSprite FireTower size lvl =
  pictures
    [ color ironGray $ rectangleSolid (size * 1.4) (size * 1.4)
    , color darkIron $ rectangleSolid (size * 1.2) (size * 1.2)
    , color (makeColor 0.8 0.3 0.2 1) $ rectangleSolid (size * 0.8) (size * 1)
    , color (makeColor 1 0.5 0.2 1) $ translate 0 (size * 0.3) $ rectangleSolid (size * 0.4) (size * 0.4)
    ]
renderTowerSprite TeslaTower size lvl =
  pictures
    [ color ironGray $ rectangleSolid (size * 1.4) (size * 1.4)
    , color darkIron $ rectangleSolid (size * 1.2) (size * 1.2)
    , color (makeColor 0.2 0.4 0.8 1) $ rectangleSolid (size * 0.6) (size * 1.2)
    , color (makeColor 0.4 0.6 1 1) $ translate 0 (size * 0.2) $ circleSolid (size * 0.2)
    ]
renderTowerSprite BallistaTower size lvl =
  pictures
    [ color woodBrown $ rectangleSolid (size * 1.5) (size * 1.5)
    , color darkBrown $ rectangleSolid (size * 1.3) (size * 1.3)
    , color ironGray $ rectangleSolid (size * 0.9) (size * 1.4)
    , color darkIron $ translate 0 (size * 0.3) $ rectangleSolid (size * 0.5) (size * 0.3)
    ]
renderTowerSprite PoisonTower size lvl =
  pictures
    [ color stoneGray $ rectangleSolid (size * 1.4) (size * 1.4)
    , color darkStoneGray $ rectangleSolid (size * 1.2) (size * 1.2)
    , color (makeColor 0.3 0.6 0.3 1) $ rectangleSolid (size * 0.8) (size * 1)
    , color (makeColor 0.5 0.8 0.5 1) $ translate 0 (size * 0.3) $ circleSolid (size * 0.3)
    ]
renderTowerSprite BombardTower size lvl =
  pictures
    [ color stoneGray $ rectangleSolid (size * 1.6) (size * 1.6)
    , color darkStoneGray $ rectangleSolid (size * 1.4) (size * 1.4)
    , color ironGray $ rectangleSolid (size * 1.0) (size * 1.2)
    , color darkIron $ translate 0 (size * 0.4) $ rectangleSolid (size * 0.6) (size * 0.4)
    ]

towerTypeColor :: TowerType -> Color
towerTypeColor ArrowTower = makeColor 0.4 0.6 0.4 1
towerTypeColor CatapultTower = makeColor 0.6 0.5 0.4 1
towerTypeColor CrossbowTower = makeColor 0.5 0.5 0.6 1
towerTypeColor FireTower = makeColor 0.8 0.3 0.2 1
towerTypeColor TeslaTower = makeColor 0.2 0.4 0.8 1
towerTypeColor BallistaTower = makeColor 0.6 0.5 0.4 1
towerTypeColor PoisonTower = makeColor 0.3 0.6 0.3 1
towerTypeColor BombardTower = makeColor 0.5 0.4 0.4 1

-- ============================================================================
-- Trap Rendering
-- ============================================================================

renderTraps :: World -> Picture
renderTraps world = pictures $ map (renderTrap (timeElapsed world)) $ M.elems (traps world)

renderTrap :: Float -> Trap -> Picture
renderTrap currentTime trap =
  let (x, y) = trapPos trap
      size = 8
      fallback = renderTrapSprite (trapType trap) size
      sprite = renderAnimatedTrapSprite (trapType trap) (trapTriggered trap) currentTime fallback
  in translate x y sprite

renderTrapSprite :: TrapType -> Float -> Picture
renderTrapSprite SpikeTrap size =
  pictures
    [ color darkStoneGray $ rectangleSolid (size * 2.2) (size * 2.2)
    , color stoneGray $ rectangleSolid (size * 1.8) (size * 1.8)
    , color darkIron $ line [(-size * 0.6, -size * 0.6), (size * 0.6, size * 0.6)]
    , color darkIron $ line [(size * 0.6, -size * 0.6), (-size * 0.6, size * 0.6)]
    , color darkIron $ translate (-size * 0.3) 0 $ circleSolid (size * 0.25)
    , color darkIron $ translate (size * 0.3) 0 $ circleSolid (size * 0.25)
    ]
renderTrapSprite FreezeTrap size =
  pictures
    [ color (makeColor 0.3 0.5 0.7 1) $ rectangleSolid (size * 2.2) (size * 2.2)
    , color (makeColor 0.4 0.6 0.9 1) $ rectangleSolid (size * 1.8) (size * 1.8)
    , color (makeColor 0.6 0.8 1 1) $ circleSolid (size * 0.6)
    , color (makeColor 0.8 0.9 1 1) $ translate 0 (size * 0.2) $ circleSolid (size * 0.3)
    ]
renderTrapSprite FirePitTrap size =
  pictures
    [ color (makeColor 0.3 0.2 0.1 1) $ rectangleSolid (size * 2.4) (size * 2.4)
    , color (makeColor 0.8 0.4 0.1 1) $ rectangleSolid (size * 1.8) (size * 1.8)
    , color (makeColor 0.9 0.5 0.2 1) $ rectangleSolid (size * 1.4) (size * 1.4)
    , color (makeColor 1 0.6 0.2 1) $ circleSolid (size * 0.6)
    ]
renderTrapSprite MagicSnareTrap size =
  pictures
    [ color (makeColor 0.5 0.3 0.6 1) $ rectangleSolid (size * 2.2) (size * 2.2)
    , color (makeColor 0.6 0.4 0.7 1) $ rectangleSolid (size * 1.8) (size * 1.8)
    , color (makeColor 0.7 0.5 0.8 1) $ circleSolid (size * 0.7)
    , color (makeColor 0.8 0.6 0.9 1) $ line [(-size * 0.5, 0), (size * 0.5, 0)]
    , color (makeColor 0.8 0.6 0.9 1) $ line [(0, -size * 0.5), (0, size * 0.5)]
    ]
renderTrapSprite ExplosiveBarrel size =
  pictures
    [ color woodBrown $ rectangleSolid (size * 2.0) (size * 2.0)
    , color darkBrown $ rectangleSolid (size * 1.6) (size * 1.6)
    , color (makeColor 0.8 0.3 0.1 1) $ translate (size * 0.3) (size * 0.3) $ circleSolid (size * 0.2)
    , color (makeColor 1 0.5 0.2 1) $ translate (size * 0.3) (size * 0.3) $ circleSolid (size * 0.1)
    ]

trapTypeColor :: TrapType -> Color
trapTypeColor SpikeTrap = makeColor 0.4 0.4 0.4 0.7
trapTypeColor FreezeTrap = makeColor 0.3 0.5 0.7 0.8
trapTypeColor FirePitTrap = makeColor 0.8 0.4 0.1 0.7
trapTypeColor MagicSnareTrap = makeColor 0.5 0.3 0.6 0.7
trapTypeColor ExplosiveBarrel = makeColor 0.7 0.2 0.2 0.7

-- ============================================================================
-- Projectile Rendering
-- ============================================================================

renderProjectiles :: World -> Picture
renderProjectiles world = pictures $ map (renderProjectile (timeElapsed world)) $ M.elems (projectiles world)

renderProjectile :: Float -> Projectile -> Picture
renderProjectile currentTime projectile =
  let (x, y) = projectilePos projectile
      size = 4
      fallback = renderProjectileSprite (projectileType projectile) size
      sprite = renderAnimatedProjectileSprite (projectileType projectile) currentTime fallback
  in translate x y sprite

renderProjectileSprite :: ProjectileType -> Float -> Picture
renderProjectileSprite Arrow size =
  color (makeColor 0.6 0.6 0.4 1) $ line [(0, 0), (size*2, 0)]
renderProjectileSprite BallistaBolt size =
  color (makeColor 0.5 0.5 0.5 1) $ rectangleSolid (size * 1.5) (size * 0.8)
renderProjectileSprite Fireball size =
  pictures
    [ color (makeColor 1 0.4 0.2 1) $ circleSolid size
    , color (makeColor 1 0.6 0.3 0.6) $ circleSolid (size * 0.6)
    ]
renderProjectileSprite IceShard size =
  color (makeColor 0.4 0.7 1 1) $ rectangleSolid (size * 0.8) (size * 2)
renderProjectileSprite LightningBolt size =
  pictures
    [ color (makeColor 1 1 0.5 1) $ line [(-size, 0), (size, 0)]
    , color (makeColor 1 1 0.7 0.6) $ line [(-size/2, size/2), (size/2, -size/2)]
    ]
renderProjectileSprite BarrageShot size =
  color (makeColor 0.7 0.6 0.5 1) $ circleSolid (size * 0.7)
renderProjectileSprite CatapultRock size =
  color (makeColor 0.4 0.4 0.3 1) $ circleSolid (size * 1.2)

projectileTypeColor :: ProjectileType -> Color
projectileTypeColor Arrow = makeColor 0.6 0.6 0.4 1
projectileTypeColor BallistaBolt = makeColor 0.5 0.5 0.5 1
projectileTypeColor Fireball = makeColor 1 0.4 0.2 1
projectileTypeColor IceShard = makeColor 0.4 0.7 1 1
projectileTypeColor LightningBolt = makeColor 1 1 0.5 1
projectileTypeColor BarrageShot = makeColor 0.7 0.6 0.5 1
projectileTypeColor CatapultRock = makeColor 0.4 0.4 0.3 1

-- ============================================================================
-- Visual Effects
-- ============================================================================

renderVisualEffects :: World -> Picture
renderVisualEffects world = pictures $ map renderEffect (visualEffects world)

renderEffect :: VisualEffect -> Picture
renderEffect (ImpactFlash pos life maxLife) =
  let (x, y) = pos
      alpha = life / maxLife
  in translate x y $ color (makeColor 1 1 0.5 alpha) $ circleSolid 15
renderEffect (ExplosionEffect pos life maxLife) =
  let (x, y) = pos
      alpha = life / maxLife
      radius = 40 * (1 - alpha)
  in translate x y $ color (makeColor 1 0.5 0.2 alpha) $ circle radius
renderEffect (FireBurst pos life) =
  let (x, y) = pos
  in translate x y $ color (makeColor 1 0.4 0.1 (life / 2)) $ circleSolid 25
renderEffect (TarSplash pos life) =
  let (x, y) = pos
  in translate x y $ color (makeColor 0.3 0.2 0.1 (life / 2)) $ circleSolid 20
renderEffect (SpikePopup pos life) =
  let (x, y) = pos
  in translate x y $ color (makeColor 0.5 0.5 0.5 life) $ rectangleSolid 8 20
renderEffect (GateFlash life) =
  translate gateX gateY $ color (makeColor 1 0.3 0.3 (life * 3)) $ rectangleSolid 20 100
renderEffect (CastleFlash life) =
  translate castleX castleY $ color (makeColor 1 0.3 0.3 (life * 3)) $ circleSolid 90
renderEffect (EnemyAttackParticle from to life) =
  let (fx, fy) = from
      (tx, ty) = to
      progress = 1.0 - (life / 0.2)  -- 0.2 is max lifetime
      px = fx + (tx - fx) * progress
      py = fy + (ty - fy) * progress
      alpha = life / 0.2
      attackColor = makeColor 1.0 0.3 0.1 alpha  -- Red-orange attack trail
  in translate px py $ color attackColor $ circleSolid 8
renderEffect (DamageNumber pos dmg life) =
  let (x, y) = pos
  in translate x (y + 20) $ color (makeColor 1 1 1 life) $ scale 0.1 0.1 $ text (show $ round dmg)

-- ============================================================================
-- Health Bar
-- ============================================================================

renderHealthBar :: Float -> Float -> Float -> Picture
renderHealthBar current max' width =
  let ratio = current / max'
      barWidth = width * 2
      barHeight = 3
      greenWidth = barWidth * ratio
  in pictures
    [ color (makeColor 0.2 0.2 0.2 1) $ rectangleSolid barWidth barHeight
    , translate (-(barWidth - greenWidth) / 2) 0 $ color (makeColor 0.2 0.8 0.2 1) $ rectangleSolid greenWidth barHeight
    ]