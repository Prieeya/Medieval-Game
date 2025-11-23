{-# LANGUAGE RecordWildCards #-}

-- Rendering.hs - All visualization and rendering code

module Rendering where

import Graphics.Gloss
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Types
import Constants
import Utils

-- ============================================================================
-- MAIN RENDER FUNCTION
-- ============================================================================

render :: GameState -> Picture
render state =
  pictures
    [ drawBackground
    , drawGrid state
    , drawCastle (castle state)
    , drawTowerRanges state
    , pictures (map (drawTowerEnhanced state) (towers state))
    , pictures (map drawEnemy (enemies state))
    , pictures (map drawProjectile (projectiles state))
    , pictures (map drawParticle (particles state))
    , drawHoverPreview state
    , drawTowerContextMenu state
    , drawUI state
    , pictures (map drawNotification (notifications state))
    ]

-- ============================================================================
-- BACKGROUND AND GRID
-- ============================================================================

drawBackground :: Picture
drawBackground = 
  pictures
    [ color (makeColor 0.15 0.2 0.15 1) (rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight))
    , color (makeColor 0.1 0.15 0.1 0.3) (pictures [translate 0 y (rectangleSolid (fromIntegral windowWidth) 3) | y <- [-300, -200..300]])
    ]

drawGrid :: GameState -> Picture
drawGrid state =
  let vLines = [Line [(x, -fromIntegral windowHeight/2), (x, fromIntegral windowHeight/2)] 
                | x <- [-fromIntegral windowWidth/2, -fromIntegral windowWidth/2 + cellSize .. fromIntegral windowWidth/2]]
      hLines = [Line [(-fromIntegral windowWidth/2, y), (fromIntegral windowWidth/2, y)] 
                | y <- [-fromIntegral windowHeight/2, -fromIntegral windowHeight/2 + cellSize .. fromIntegral windowHeight/2]]
      gridLines = color (makeColor 0.3 0.4 0.3 0.15) (pictures (vLines ++ hLines))
      
      dangerZoneVis = pictures [
        let (x, y) = gridToWorld pos
            intensity = min 1.0 (val / 3.0)
        in translate x y (color (makeColor 1 0 0 (intensity * 0.2)) (rectangleSolid cellSize cellSize))
        | (pos, val) <- Map.toList (dangerZones (aiMemory state))]
      
  in pictures [gridLines, dangerZoneVis]

-- ============================================================================
-- CASTLE RENDERING
-- ============================================================================

drawCastle :: Castle -> Picture
drawCastle c =
  let (x, y) = castlePos c
      healthPercent = fromIntegral (castleHealth c) / fromIntegral (castleMaxHealth c)
      shieldAlpha = fromIntegral (castleShield c) / 20.0
  in pictures
       [ translate x y (color (dark red) (rectangleSolid 70 70))
       , translate x y (color red (rectangleSolid 60 60))
       , translate (x-25) (y+25) (color (dark red) (rectangleSolid 15 30))
       , translate (x+25) (y+25) (color (dark red) (rectangleSolid 15 30))
       , translate x (y+35) (color yellow (Polygon [(0,0), (10,5), (0,10)]))
       , if castleShield c > 0
           then translate x y (color (makeColor 0.3 0.6 1 (shieldAlpha * 0.4)) (ThickCircle 35 5))
           else blank
       , translate x (y-50) (scale 0.12 0.12 (color white (text "CASTLE")))
       , translate x (y-65) (color (dark red) (rectangleSolid 70 8))
       , translate (x - 35 + 35 * healthPercent) (y-65) (color green (rectangleSolid (70 * healthPercent) 8))
       , if castleShield c > 0
           then translate x (y-75) (color cyan (rectangleSolid (70 * fromIntegral (castleShield c) / 20) 5))
           else blank
       ]

-- ============================================================================
-- TOWER RENDERING
-- ============================================================================

drawArrowTower :: Int -> Picture
drawArrowTower level =
  let baseColor = makeColor 0.5 0.5 0.5 1
      accentColor = makeColor 0.7 0.5 0.3 1
      size = 18 + fromIntegral (level - 1) * 2
  in pictures
       [ color baseColor (rectangleSolid size size)
       , color baseColor (translate (-size/2) (size/2) (rectangleSolid (size/4) (size/4)))
       , color baseColor (translate (size/2) (size/2) (rectangleSolid (size/4) (size/4)))
       , color baseColor (translate 0 (size/2) (rectangleSolid (size/4) (size/4)))
       , color black (rectangleSolid 3 (size * 0.6))
       , color accentColor (translate 0 (-size/3) (rectangleSolid (size * 0.8) 3))
       , color white (Polygon [(0, size/2 + 3), (-3, size/2 - 2), (3, size/2 - 2)])
       ]

drawCannonTower :: Int -> Picture
drawCannonTower level =
  let baseColor = makeColor 0.4 0.3 0.2 1
      metalColor = makeColor 0.6 0.6 0.6 1
      size = 20 + fromIntegral (level - 1) * 2
  in pictures
       [ color baseColor (rectangleSolid (size * 1.2) (size * 1.2))
       , color (dark baseColor) (rectangleSolid (size * 1.1) (size * 1.1))
       , color metalColor (rectangleSolid (size * 1.3) 6)
       , color (dark metalColor) (rectangleSolid (size * 1.3) 4)
       , translate (size * 0.65) 0 (color metalColor (circleSolid 7))
       , translate (size * 0.65) 0 (color black (circleSolid 4))
       , color metalColor (translate (-size/2) (size/2) (rectangleSolid 4 4))
       , color metalColor (translate (size/2) (size/2) (rectangleSolid 4 4))
       , color metalColor (translate (-size/2) (-size/2) (rectangleSolid 4 4))
       , color metalColor (translate (size/2) (-size/2) (rectangleSolid 4 4))
       ]

drawIceTower :: Int -> Picture
drawIceTower level =
  let baseColor = makeColor 0.6 0.8 1 0.9
      glowColor = makeColor 0.3 0.6 1 0.6
      size = 18 + fromIntegral (level - 1) * 2
  in pictures
       [ color glowColor (circleSolid (size * 1.5))
       , color baseColor (Polygon [(0, size * 1.2), (size * 0.7, 0), (0, -size * 1.2), (-size * 0.7, 0)])
       , color (makeColor 0.8 0.9 1 1) (Polygon [(0, size * 0.8), (size * 0.5, 0), (0, -size * 0.8), (-size * 0.5, 0)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(0, size * 1.2), (0, -size * 1.2)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(-size * 0.7, 0), (size * 0.7, 0)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(-size * 0.5, size * 0.8), (size * 0.5, -size * 0.8)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(size * 0.5, size * 0.8), (-size * 0.5, -size * 0.8)])
       , color white (circleSolid (size * 0.3))
       ]

drawLightningTower :: Int -> Picture
drawLightningTower level =
  let baseColor = makeColor 0.8 0.3 1 1
      sparkColor = makeColor 1 1 0.5 1
      size = 20 + fromIntegral (level - 1) * 2
  in pictures
       [ color (makeColor 0.6 0.2 0.8 0.5) (ThickCircle (size * 1.2) 3)
       , color baseColor (ThickCircle size 4)
       , color (makeColor 0.9 0.5 1 1) (circleSolid (size * 0.7))
       , color sparkColor (Polygon [(0, size * 0.7), (-3, size * 0.3), (0, size * 0.2), (3, -size * 0.7)])
       , rotate 120 (color sparkColor (Polygon [(0, size * 0.5), (-2, size * 0.2), (0, 0), (2, -size * 0.5)]))
       , rotate 240 (color sparkColor (Polygon [(0, size * 0.5), (-2, size * 0.2), (0, 0), (2, -size * 0.5)]))
       , color white (circleSolid (size * 0.2))
       ]

drawLevelBanners :: Int -> Float -> Float -> Picture
drawLevelBanners level x y =
  let bannerPositions = [(-12, 18), (0, 20), (12, 18)]
      activeBanners = take level bannerPositions
      bannerColor = makeColor 0.9 0.7 0.1 1
  in pictures [translate (x + dx) (y + dy) (pictures
                 [ color bannerColor (Polygon [(0, 0), (6, -3), (0, -8), (-6, -3)])
                 , color (dark bannerColor) (Line [(0, 0), (0, -8)])
                 ]) | (dx, dy) <- activeBanners]

drawTowerEnhanced :: GameState -> Tower -> Picture
drawTowerEnhanced state t =
  let (x, y) = towerPos t
      level = towerLevel t
      
      showRange = case hoveredCell state of
        Just cell -> worldToGrid (x, y) == cell
        Nothing -> False
      
      rangeCircle = if showRange
        then translate x y (color (makeColor 1 1 1 0.15) (ThickCircle (getTowerRange t) 2))
        else blank
      
      basePlatform = pictures
        [ color (makeColor 0.3 0.25 0.2 1) (rectangleSolid 32 32)
        , color (makeColor 0.4 0.35 0.3 1) (rectangleSolid 28 28)
        ]
      
      towerStructure = case towerType t of
        Arrow -> drawArrowTower level
        Cannon -> drawCannonTower level
        Ice -> drawIceTower level
        Lightning -> drawLightningTower level
      
      levelIndicators = drawLevelBanners level x y
      
      killBadge = if towerKills t > 0
        then translate x (y - 22) (pictures
               [ color (makeColor 0.8 0.7 0.1 0.8) (circleSolid 8)
               , scale 0.06 0.06 (color black (text (show (towerKills t))))
               ])
        else blank
      
  in pictures [rangeCircle, translate x y basePlatform, translate x y towerStructure, levelIndicators, killBadge]

drawTowerRanges :: GameState -> Picture
drawTowerRanges state =
  case (selectedTool state, hoveredCell state) of
    (Just ttype, Just cell) ->
      let (x, y) = gridToWorld cell
          range = baseTowerRange ttype
      in translate x y (color (makeColor 1 1 1 0.1) (ThickCircle range 2))
    _ -> blank

-- ============================================================================
-- TOWER CONTEXT MENU
-- ============================================================================

drawTowerContextMenu :: GameState -> Picture
drawTowerContextMenu state =
  case hoveredCell state of
    Nothing -> blank
    Just cell ->
      let nearbyTowers = filter (\t -> worldToGrid (towerPos t) == cell) (towers state)
      in case nearbyTowers of
           [] -> blank
           (tower:_) ->
             let (tx, ty) = towerPos tower
                 menuX = tx + 40
                 menuY = ty
                 
                 menuBg = translate menuX menuY (color (makeColor 0.1 0.1 0.15 0.95) (rectangleSolid 140 120))
                 
                 towerName = case towerType tower of
                   Arrow -> "Arrow Tower"
                   Cannon -> "Cannon Tower"
                   Ice -> "Ice Tower"
                   Lightning -> "Lightning Tower"
                 
                 titleText = translate (menuX - 65) (menuY + 50) 
                            (scale 0.12 0.12 (color white (text towerName)))
                 
                 levelText = translate (menuX - 65) (menuY + 35) 
                            (scale 0.1 0.1 (color yellow (text ("Level: " ++ show (towerLevel tower)))))
                 
                 killsText = translate (menuX - 65) (menuY + 22) 
                            (scale 0.1 0.1 (color (makeColor 1 0.5 0.5 1) (text ("Kills: " ++ show (towerKills tower)))))
                 
                 rangeText = translate (menuX - 65) (menuY + 9) 
                            (scale 0.08 0.08 (color (greyN 0.7) (text ("Range: " ++ show (floor (getTowerRange tower) :: Int)))))
                 
                 upgradeCost = 50 * towerLevel tower
                 canUpgrade = towerLevel tower < 3 && gold (resources state) >= upgradeCost
                 
                 upgradeButton = if towerLevel tower < 3
                   then pictures
                     [ translate menuX (menuY - 10) (color (if canUpgrade 
                         then makeColor 0.2 0.6 0.2 0.9 
                         else makeColor 0.3 0.3 0.3 0.7) (rectangleSolid 120 22))
                     , translate (menuX - 55) (menuY - 12) 
                       (scale 0.1 0.1 (color (if canUpgrade then white else greyN 0.5) 
                       (text ("[U] " ++ show upgradeCost ++ "g"))))
                     ]
                   else translate (menuX - 50) (menuY - 12) 
                        (scale 0.09 0.09 (color (makeColor 1 0.8 0 1) (text "MAX LEVEL")))
                 
                 sellValue = (towerCost (towerType tower) + 50 * (towerLevel tower - 1)) `div` 2
                 sellButton = pictures
                   [ translate menuX (menuY - 40) (color (makeColor 0.6 0.2 0.2 0.9) (rectangleSolid 120 22))
                   , translate (menuX - 55) (menuY - 42) 
                     (scale 0.1 0.1 (color white (text ("[S] +" ++ show sellValue ++ "g"))))
                   ]
                 
                 connectionLine = color (makeColor 1 1 1 0.3) (Line [(tx, ty), (menuX - 70, menuY)])
                 
             in pictures [connectionLine, menuBg, titleText, levelText, killsText, rangeText, upgradeButton, sellButton]

-- ============================================================================
-- ENEMY RENDERING
-- ============================================================================

drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
      healthPercent = fromIntegral (enemyHealth e) / fromIntegral (enemyMaxHealth e)
      (col, size) = case enemyType e of
        BasicEnemy -> (red, 12)
        FastEnemy -> (makeColor 1 0.5 0.5 1, 9)
        TankEnemy -> (makeColor 0.6 0.1 0.1 1, 18)
        BossEnemy -> (makeColor 0.8 0 0 1, 25)
      
      stateColor = case enemyState e of
        Retreating -> makeColor 0.5 0.5 1 1
        Regrouping -> yellow
        _ -> col
      
      enemyCircle = color stateColor (circleSolid size)
      
      armorBars = if enemyArmor e > 0
        then translate x (y - size - 5) (color (greyN 0.7) (rectangleSolid 20 3))
        else blank
      
      healthBg = translate x (y + size + 5) (color (dark red) (rectangleSolid 24 4))
      healthBar = translate (x - 12 + 12 * healthPercent) (y + size + 5) 
                   (color green (rectangleSolid (24 * healthPercent) 4))
      
      stunEffect = if enemyStunned e > 0
        then pictures [translate x y (color cyan (ThickCircle (size * 1.3) 2)),
                      translate x (y + size + 15) (scale 0.08 0.08 (color cyan (text "STUNNED")))]
        else blank
      
  in pictures [translate x y enemyCircle, armorBars, healthBg, healthBar, stunEffect]

-- ============================================================================
-- PROJECTILE RENDERING
-- ============================================================================

drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
      (col, size, shape) = case projType p of
        Arrow -> (yellow, 4, circleSolid 4)
        Cannon -> (orange, 6, circleSolid 6)
        Ice -> (cyan, 5, Polygon [(0,6), (4,-3), (-4,-3)])
        Lightning -> (makeColor 1 1 0.5 1, 5, ThickCircle 5 3)
      
      trail = color (makeColor 1 1 1 0.3) (Line [(x, y), (x - 10, y)])
      
  in pictures [trail, translate x y (color col shape)]

-- ============================================================================
-- PARTICLE RENDERING
-- ============================================================================

drawFloatingTextParticle :: Particle -> Picture
drawFloatingTextParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p / 1.2)
      fadeColor = partColor p
      txt = fromMaybe "" (partText p)
      scaleFactor = partSize p * 0.01
      (r, g, b, _) = rgbaOfColor fadeColor
  in pictures
       [ translate (x + 2) (y - 2) (scale scaleFactor scaleFactor (color (makeColor 0 0 0 (alpha * 0.5)) (text txt)))
       , translate x y (scale scaleFactor scaleFactor (color (makeColor r g b alpha) (text txt)))
       ]

drawCircleParticle :: Particle -> Picture
drawCircleParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p)
      (r, g, b, _) = rgbaOfColor (partColor p)
      fadedColor = makeColor r g b alpha
  in translate x y (color fadedColor (circleSolid (partSize p)))

drawSparkParticle :: Particle -> Picture
drawSparkParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p)
      (r, g, b, _) = rgbaOfColor (partColor p)
      fadedColor = makeColor r g b alpha
      (vx, vy) = partVel p
      angle = atan2 vy vx
  in translate x y (rotate (angle * 180 / pi) (color fadedColor (rectangleSolid (partSize p * 3) (partSize p * 0.5))))

drawSmokeParticle :: Particle -> Picture
drawSmokeParticle p =
  let (x, y) = partPos p
      alpha = min 0.4 (partLife p * 0.4)
      size = partSize p * (2 - partLife p)
  in translate x y (color (makeColor 0.5 0.5 0.5 alpha) (circleSolid size))

drawParticle :: Particle -> Picture
drawParticle p =
  case partType p of
    TextParticle -> drawFloatingTextParticle p
    CircleParticle -> drawCircleParticle p
    SparkParticle -> drawSparkParticle p
    SmokeParticle -> drawSmokeParticle p

-- ============================================================================
-- UI RENDERING
-- ============================================================================

drawHoverPreview :: GameState -> Picture
drawHoverPreview state =
  case (selectedTool state, hoveredCell state) of
    (Just ttype, Just cell) ->
      let (x, y) = gridToWorld cell
          col = case ttype of
            Arrow -> makeColor 0.2 0.4 1 0.5
            Cannon -> makeColor 1 0.5 0 0.5
            Ice -> makeColor 0.4 0.8 1 0.5
            Lightning -> makeColor 0.8 0.2 1 0.5
          cost = towerCost ttype
          canAfford = gold (resources state) >= cost
          previewCol = if canAfford then col else makeColor 1 0 0 0.3
      in translate x y (color previewCol (circleSolid 15))
    _ -> blank

drawUI :: GameState -> Picture
drawUI state =
  let panelBg = translate (-420) 280 (color (makeColor 0.1 0.1 0.15 0.9) (rectangleSolid 140 120))
      
      goldText = translate (-480) 310 (scale 0.15 0.15 (color yellow (text ("Gold: " ++ show (gold (resources state))))))
      manaText = translate (-480) 290 (scale 0.15 0.15 (color cyan (text ("Mana: " ++ show (mana (resources state))))))
      waveText = translate (-480) 270 (scale 0.15 0.15 (color white (text ("Wave: " ++ show (wave state) ++ "/15"))))
      hpText = translate (-480) 250 (scale 0.15 0.15 (color green (text ("Castle HP: " ++ show (castleHealth (castle state))))))
      
      towerButtons = pictures
        [ drawTowerButton state (-480) 220 Arrow "1"
        , drawTowerButton state (-480) 195 Cannon "2"
        , drawTowerButton state (-480) 170 Ice "3"
        , drawTowerButton state (-480) 145 Lightning "4"
        ]
      
      helpText = translate (-480) 115 (scale 0.08 0.08 (color (greyN 0.7) (text "[U] Upgrade [S] Sell")))
      
      waveTimerDisplay = if null (enemies state) && waveTimer state > 0
        then translate 0 (-300) (scale 0.2 0.2 (color yellow (text ("Next wave in: " ++ show (ceiling (waveTimer state) :: Int)))))
        else blank
      
  in pictures
       [ panelBg
       , goldText
       , manaText
       , waveText
       , hpText
       , towerButtons
       , helpText
       , waveTimerDisplay
       , if gameOver state
         then pictures
                [ color (makeColor 0 0 0 0.8) (rectangleSolid 500 300)
                , translate (-150) 40 (scale 0.4 0.4 (color red (text "DEFEAT")))
                , translate (-180) (-20) (scale 0.18 0.18 (color white (text ("Reached Wave " ++ show (wave state)))))
                , translate (-150) (-60) (scale 0.15 0.15 (color white (text "Click to restart")))
                ]
         else blank
       , if victory state
         then pictures
                [ color (makeColor 0 0 0 0.8) (rectangleSolid 500 300)
                , translate (-180) 40 (scale 0.4 0.4 (color green (text "VICTORY!")))
                , translate (-200) (-20) (scale 0.18 0.18 (color white (text "Castle defended!")))
                , translate (-150) (-60) (scale 0.15 0.15 (color white (text "Click to restart")))
                ]
         else blank
       ]

drawTowerButton :: GameState -> Float -> Float -> TowerType -> String -> Picture
drawTowerButton state x y ttype key =
  let cost = towerCost ttype
      canAfford = gold (resources state) >= cost
      isSelected = selectedTool state == Just ttype
      
      bgCol = if isSelected
              then makeColor 0.3 0.5 0.8 0.8
              else if canAfford
                   then makeColor 0.2 0.3 0.4 0.6
                   else makeColor 0.3 0.1 0.1 0.6
      
      nameCol = if canAfford then white else greyN 0.5
      
      name = case ttype of
        Arrow -> "Arrow"
        Cannon -> "Cannon"
        Ice -> "Ice"
        Lightning -> "Lightning"
      
  in pictures
       [ translate (x + 60) y (color bgCol (rectangleSolid 110 20))
       , translate x y (scale 0.1 0.1 (color nameCol (text ("[" ++ key ++ "] " ++ name ++ " " ++ show cost ++ "g"))))
       ]

drawNotification :: Notification -> Picture
drawNotification notif =
  translate 0 (280 - notifPos notif) (scale 0.15 0.15 (color (notifColor notif) (text (notifText notif))))