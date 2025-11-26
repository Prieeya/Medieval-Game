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
import Rendering.PixelArt (renderPixelTower, renderPixelEnemy, renderPixelTrap, pixelColor, renderPixelWall, renderPixelGate, renderPixelFort, renderPixelCastle)
import Rendering.SpriteAnimation (renderAnimatedEnemy, renderAnimatedTower, renderAnimatedTrap, renderAnimatedProjectile, renderGrassTile, renderPathTile)
import Constants (leftSpawnX, centerSpawnX, rightSpawnX, gateX, gateY, fortCenterX, fortWidth, fortHeight)

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
  , renderDecorations world  -- Decorations above path, below towers
  , renderFort world
  , renderCastle world
  , renderDeploymentPreview world
  , renderTraps world
  , renderTowers world  -- Towers above decorations
  , renderEnemies world  -- Enemies above towers
  , renderProjectiles world  -- Projectiles highest
  , renderVisualEffects world
  ]

-- ============================================================================
-- Decoration Rendering
-- ============================================================================

renderDecorations :: World -> Picture
renderDecorations world = pictures $ map renderDecoration $ M.elems (decorations world)

renderDecoration :: Decoration -> Picture
renderDecoration deco =
  let (x, y) = decoPos deco
      size = 32  -- Base size for decorations
  in translate x y $ case decoType deco of
    TreeSmall -> renderTreeSmall size
    TreeLarge -> renderTreeLarge size
    Bush -> renderBush size
    Rock -> renderRock size

renderTreeSmall :: Float -> Picture
renderTreeSmall size =
  pictures
    [ color (makeColor 0.2 0.5 0.1 1) $ circleSolid (size * 0.6)  -- Foliage
    , color (makeColor 0.3 0.2 0.1 1) $ translate 0 (-size * 0.3) $ rectangleSolid (size * 0.2) (size * 0.4)  -- Trunk
    ]

renderTreeLarge :: Float -> Picture
renderTreeLarge size =
  pictures
    [ color (makeColor 0.2 0.5 0.1 1) $ circleSolid (size * 0.8)  -- Foliage
    , color (makeColor 0.3 0.2 0.1 1) $ translate 0 (-size * 0.4) $ rectangleSolid (size * 0.3) (size * 0.5)  -- Trunk
    ]

renderBush :: Float -> Picture
renderBush size =
  color (makeColor 0.2 0.4 0.1 1) $ circleSolid (size * 0.4)

renderRock :: Float -> Picture
renderRock size =
  color (makeColor 0.4 0.4 0.4 1) $ circleSolid (size * 0.3)

-- ============================================================================
-- Background
-- ============================================================================

renderBackground :: Picture
renderBackground = 
  -- Tile-based grass field with pixel art tiles
  let tileSize = 64.0  -- 64x64 tiles
      tilesX = floor (worldWidth / tileSize) + 1
      tilesY = floor (worldHeight / tileSize) + 1
      -- Render grass tiles
      grassTiles = pictures $ concatMap (\iy ->
        concatMap (\ix ->
          let tileX = worldLeft + fromIntegral ix * tileSize + tileSize/2
              tileY = worldBottom + fromIntegral iy * tileSize + tileSize/2
          in [renderGrassTile tileX tileY]
          ) [0..tilesX]
        ) [0..tilesY]
      
      -- Path from spawn area to fort gate using path tiles
      pathTiles = pictures $ concatMap (\i ->
        let t = fromIntegral i / 30.0
            startX = leftSpawnX + 100
            startY = 0
            endX = gateX - 50
            endY = gateY
            midX = (startX + endX) / 2
            midY = startY + 100
            x = (1-t)*(1-t)*startX + 2*(1-t)*t*midX + t*t*endX
            y = (1-t)*(1-t)*startY + 2*(1-t)*t*midY + t*t*endY
            -- Snap to tile grid
            tileX = (fromIntegral (floor (x / tileSize))) * tileSize + tileSize/2
            tileY = (fromIntegral (floor (y / tileSize))) * tileSize + tileSize/2
        in [renderPathTile tileX tileY]
        ) [0..30]
      
      -- Center and right paths
      centerPathTiles = pictures $ concatMap (\i ->
        let t = fromIntegral i / 20.0
            startX = centerSpawnX + 50
            startY = 0
            endX = gateX - 30
            endY = gateY - 50
            midX = (startX + endX) / 2
            midY = startY - 80
            x = (1-t)*(1-t)*startX + 2*(1-t)*t*midX + t*t*endX
            y = (1-t)*(1-t)*startY + 2*(1-t)*t*midY + t*t*endY
            tileX = (fromIntegral (floor (x / tileSize))) * tileSize + tileSize/2
            tileY = (fromIntegral (floor (y / tileSize))) * tileSize + tileSize/2
        in [renderPathTile tileX tileY]
        ) [0..20]
      
      rightPathTiles = pictures $ concatMap (\i ->
        let t = fromIntegral i / 20.0
            startX = rightSpawnX + 50
            startY = 0
            endX = gateX - 30
            endY = gateY + 50
            midX = (startX + endX) / 2
            midY = startY + 80
            x = (1-t)*(1-t)*startX + 2*(1-t)*t*midX + t*t*endX
            y = (1-t)*(1-t)*startY + 2*(1-t)*t*midY + t*t*endY
            tileX = (fromIntegral (floor (x / tileSize))) * tileSize + tileSize/2
            tileY = (fromIntegral (floor (y / tileSize))) * tileSize + tileSize/2
        in [renderPathTile tileX tileY]
        ) [0..20]
      
  in pictures
    [ grassTiles
    , pathTiles
    , centerPathTiles
    , rightPathTiles
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
  [ renderPixelFort fortCenterX 0 fortWidth fortHeight  -- Fort interior ground
  , renderWalls (fortWalls $ fort world)
  , renderGate (fortGate $ fort world)
  , renderCornerTowers  -- Add decorative corner towers
  ]

-- Render decorative conical corner towers on fort
renderCornerTowers :: Picture
renderCornerTowers = pictures
  [ renderCornerTower (fortLeft, fortTop)  -- Top-left corner
  , renderCornerTower (fortRight, fortTop)  -- Top-right corner
  , renderCornerTower (fortLeft, fortBottom)  -- Bottom-left corner
  , renderCornerTower (fortRight, fortBottom)  -- Bottom-right corner
  ]

-- Render a single conical corner tower
renderCornerTower :: (Float, Float) -> Picture
renderCornerTower (x, y) =
  let towerSize = 40
      towerHeight = 60
      pixelSize = 4
      -- Base of tower (square)
      basePixels = [(-2, -3, "gray"), (-1, -3, "dark gray"), (0, -3, "gray"), (1, -3, "dark gray"), (2, -3, "gray")
                   , (-2, -2, "dark gray"), (-1, -2, "gray"), (0, -2, "dark gray"), (1, -2, "gray"), (2, -2, "dark gray")
                   , (-2, -1, "gray"), (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray"), (2, -1, "gray")
                   , (-2, 0, "dark gray"), (-1, 0, "gray"), (0, 0, "dark gray"), (1, 0, "gray"), (2, 0, "dark gray")
                   , (-2, 1, "gray"), (-1, 1, "dark gray"), (0, 1, "gray"), (1, 1, "dark gray"), (2, 1, "gray")
                   ]
      -- Tower body (vertical)
      bodyPixels = [(-1, 2, "gray"), (0, 2, "dark gray"), (1, 2, "gray")
                   , (-1, 3, "dark gray"), (0, 3, "gray"), (1, 3, "dark gray")
                   , (-1, 4, "gray"), (0, 4, "dark gray"), (1, 4, "gray")
                   , (-1, 5, "dark gray"), (0, 5, "gray"), (1, 5, "dark gray")
                   , (-1, 6, "gray"), (0, 6, "dark gray"), (1, 6, "gray")
                   ]
      -- Conical roof (pointed)
      roofPixels = [(-2, 7, "dark gray"), (-1, 7, "gray"), (0, 7, "dark gray"), (1, 7, "gray"), (2, 7, "dark gray")
                   , (-1, 8, "gray"), (0, 8, "dark gray"), (1, 8, "gray")
                   , (0, 9, "dark gray")
                   ]
      -- Battlements on top
      battlementPixels = [(-2, 6, "gray"), (2, 6, "gray")]
      allPixels = basePixels ++ bodyPixels ++ roofPixels ++ battlementPixels
  in translate x y $ pictures $ map (\(px, py, col) -> translate (px * pixelSize) (py * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) allPixels
  where
    pixelColor = Rendering.PixelArt.pixelColor

renderWalls :: [WallSegment] -> Picture
renderWalls walls = pictures $ map renderWall walls

renderWall :: WallSegment -> Picture
renderWall wall =
  let (x1, y1) = wallStart wall
      (x2, y2) = wallEnd wall
      healthRatio = wallHP wall / (Types.wallMaxHP wall)
  in renderPixelWall x1 y1 x2 y2 healthRatio

renderGate :: Gate -> Picture
renderGate gate =
  let (x, y) = gatePos gate
      gateHeight = Types.gateWidth gate
      gateWidth = 60  -- Wider gate
  in pictures
    [ renderPixelGate x y gateWidth gateHeight (gateDestroyed gate)
    , translate x (y + gateHeight/2 + 12) $ renderHealthBar (gateHP gate) (Types.gateMaxHP gate) 50
    ]

-- ============================================================================
-- Castle Rendering
-- ============================================================================

renderCastle :: World -> Picture
renderCastle world =
  let c = castle world
      (x, y) = castlePos c
      size = Types.castleSize c * 4.0  -- Much larger castle matching JSON description
  in pictures
    [ renderPixelCastle x y size
    , translate x (y + size/2 + 20) $ renderHealthBar (castleHP c) (Types.castleMaxHP c) (size * 0.8)
    ]

-- ============================================================================
-- Enemy Rendering
-- ============================================================================

renderEnemies :: World -> Picture
renderEnemies world = pictures $ map (renderEnemy (timeElapsed world)) $ M.elems (enemies world)

renderEnemy :: Float -> Enemy -> Picture
renderEnemy currentTime enemy =
  let (x, y) = enemyPos enemy
      flashColor = if enemyHitFlash enemy > 0
                   then makeColor 1 1 1 (enemyHitFlash enemy * 5)
                   else makeColor 0 0 0 0
      baseSize = enemySizeByRole (enemyRole enemy)
      -- Apply scaling: 40% bigger
      size = baseSize * globalPixelScale * enemyScale
      -- Bosses get additional scaling
      finalSize = if enemyRole enemy == Boss
                  then size * (bossScale / enemyScale)  -- Already scaled by enemyScale, so multiply by ratio
                  else size
      healthBar = renderHealthBar (enemyHP enemy) (enemyMaxHP enemy) finalSize
      -- Use animated sprite rendering with scaled size
      sprite = renderAnimatedEnemy (enemyType enemy) (enemyAnimState enemy) finalSize
  in translate x y $ pictures
    [ sprite
    , color flashColor $ circleSolid (finalSize + 2)
    , translate 0 (finalSize + 8) healthBar
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

-- renderEnemySprite removed - now using renderAnimatedEnemy from SpriteAnimation

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
enemySizeByRole Melee = 64  -- 64x64 base resolution as per JSON
enemySizeByRole Fast = 64  -- 64x64 base resolution
enemySizeByRole Ranged = 64  -- 64x64 base resolution
enemySizeByRole Heavy = 80  -- Larger for heavy units
enemySizeByRole Siege = 80  -- Larger for siege units
enemySizeByRole Boss = 128  -- Much larger for bosses

-- ============================================================================
-- Tower Rendering
-- ============================================================================

renderTowers :: World -> Picture
renderTowers world = pictures $ map (renderTower (timeElapsed world)) $ M.elems (towers world)

renderTower :: Float -> Tower -> Picture
renderTower currentTime tower =
  let (x, y) = towerPos tower
      baseSize = 64 + fromIntegral (towerLevel tower) * 8  -- 64x64 base resolution
      -- Apply scaling: 40% bigger
      size = baseSize * globalPixelScale * towerScale
      range = towerRange tower
      -- NO range indicator after placement (only show during preview)
      healthBar = renderHealthBar (towerHP tower) (Types.towerMaxHP tower) size
      -- Use animated sprite rendering with scaled size
      sprite = renderAnimatedTower (towerType tower) (towerAnimState tower) size
  in translate x y $ pictures
    [ sprite
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

-- Render deployment preview with red transparent arch BEFORE placement
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
          -- RED transparent arch (50% alpha) before placement - pointing left toward enemies
          archAngle = 120  -- 120 degree arch
          archSegments = 30
          startAngle = 180 - archAngle / 2  -- Start from left side
          angleStep = archAngle / fromIntegral archSegments
          archPoints = map (\i ->
            let a = startAngle + angleStep * fromIntegral i
                rad = a * pi / 180
            in (range * cos rad, range * sin rad)
            ) [0..archSegments]
          archLine = line archPoints
          archColor = makeColor 1.0 0.0 0.0 0.5  -- Red transparent
          archOutline = makeColor 1.0 0.0 0.0 0.7  -- Red outline
          -- Lines from center to arch ends
          endPoint1 = head archPoints
          endPoint2 = last archPoints
          centerLines = pictures
            [ line [(0, 0), endPoint1]
            , line [(0, 0), endPoint2]
            ]
          previewTower = color (if isValid then makeColor 0.2 1.0 0.2 0.6 else makeColor 1.0 0.2 0.2 0.6) $ circleSolid 15
      in translate mx my $ pictures
        [ color archColor archLine
        , color archOutline archLine
        , color archColor centerLines
        , previewTower
        ]
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
      baseSize = 64  -- 64x64 base resolution as per JSON
      -- Apply scaling: smaller defenses
      size = baseSize * globalPixelScale * towerScale  -- Use towerScale for consistency
      -- Use animated sprite rendering
      sprite = renderAnimatedTrap (trapType trap) (trapAnimState trap) size
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
      baseSize = 16
      -- Apply scaling: 5x bigger (3.5x from projectileScale)
      size = baseSize * globalPixelScale * projectileScale
      -- Use animated sprite rendering with scaled size
      sprite = renderAnimatedProjectile (projectileType projectile) (projectileAnimState projectile) size
      -- Add smaller white/yellow outline for visibility (reduced radius)
      outlineRadius = size * 0.15  -- Much smaller outline radius (15% of size)
      outline = color (makeColor 1.0 1.0 0.8 0.8) $ circleSolid outlineRadius
      outline2 = color (makeColor 1.0 1.0 1.0 0.6) $ circleSolid (outlineRadius * 0.8)
  in translate x y $ pictures
    [ outline2
    , outline
    , sprite
    ]

renderProjectileSprite :: ProjectileType -> Float -> Picture
renderProjectileSprite Arrow size =
  let pixelSize = size / 2
      pixels = [(0, 0, "tan"), (1, 0, "brown"), (2, 0, "tan")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite BallistaBolt size =
  let pixelSize = size / 2
      pixels = [(0, 0, "silver"), (1, 0, "gray"), (2, 0, "silver")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite Fireball size =
  let pixelSize = size / 2
      pixels = [(-1, 0, "orange"), (0, 0, "yellow"), (1, 0, "orange"), (0, -1, "red"), (0, 1, "red")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite IceShard size =
  let pixelSize = size / 2
      pixels = [(0, -1, "light blue"), (0, 0, "blue"), (0, 1, "light blue"), (0, 2, "white")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite LightningBolt size =
  let pixelSize = size / 2
      pixels = [(-1, 0, "yellow"), (0, 0, "white"), (1, 0, "yellow"), (0, -1, "blue"), (0, 1, "blue")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite CatapultRock size =
  let pixelSize = size / 2
      pixels = [(-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray"), (-1, 1, "gray"), (0, 1, "dark gray"), (1, 1, "gray")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels
renderProjectileSprite BarrageShot size =
  let pixelSize = size / 2
      pixels = [(-1, -1, "brown"), (0, -1, "tan"), (1, -1, "brown"), (-1, 0, "tan"), (0, 0, "brown"), (1, 0, "tan"), (-1, 1, "brown"), (0, 1, "tan"), (1, 1, "brown")]
  in pictures $ map (\(x, y, col) -> translate (x * pixelSize) (y * pixelSize) $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) pixels

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
      barWidth = width * 1.5  -- Reduced from 2.0
      barHeight = 2  -- Reduced from 3
      greenWidth = barWidth * ratio
  in pictures
    [ color (makeColor 0.2 0.2 0.2 1) $ rectangleSolid barWidth barHeight
    , translate (-(barWidth - greenWidth) / 2) 0 $ color (makeColor 0.2 0.8 0.2 1) $ rectangleSolid greenWidth barHeight
    ]