module Rendering.RenderUI where

import Graphics.Gloss
import Types
import Constants
import qualified Data.Map.Strict as M
import Rendering.PixelArt (pixelColor)
import qualified Constants as C

-- ============================================================================
-- UI Rendering - Medieval Pixel Art Theme
-- ============================================================================

-- Medieval color palette
woodBrown :: Color
woodBrown = pixelColor "brown"

darkWoodBrown :: Color
darkWoodBrown = pixelColor "dark brown"

stoneGray :: Color
stoneGray = pixelColor "gray"

darkStoneGray :: Color
darkStoneGray = pixelColor "dark gray"

parchment :: Color
parchment = makeColor 0.95 0.9 0.8 1.0

metalSilver :: Color
metalSilver = pixelColor "silver"

goldColor :: Color
goldColor = makeColor 1.0 0.84 0.0 1.0

-- ============================================================================
-- Main UI Layout
-- ============================================================================

renderUI :: World -> Picture
renderUI world = pictures
  [ renderTopBar world
  , renderTreasuryPanel world
  , renderLevelPanel world
  , renderCastleHPBar world
  , renderTowerHotbar world
  , renderTrapHotbar world
  , renderBuildModeDisplay world
  , renderGameStatus world
  ]

-- ============================================================================
-- Top Bar - Wooden Plank Header with Metal Rivets
-- ============================================================================

renderTopBar :: World -> Picture
renderTopBar world =
  let barWidth = worldWidth
      barHeight = 40
      barY = worldHeight / 2 - barHeight / 2
      -- Wooden plank texture
      woodPattern = pictures $ map (\i ->
        let x = -worldWidth/2 + fromIntegral (i `mod` 20) * (worldWidth / 20)
            y = barY
            woodColor = if i `mod` 2 == 0 then woodBrown else darkWoodBrown
        in translate x y $ color woodColor $ rectangleSolid (worldWidth / 20) barHeight
        ) [0..19]
      -- Metal rivets
      rivets = pictures $ map (\i ->
        let x = -worldWidth/2 + 20 + fromIntegral i * 150
            y = barY
        in translate x y $ color metalSilver $ circleSolid 3
        ) [0..10]
      -- Border
      border = translate 0 barY $ color darkWoodBrown $ rectangleWire barWidth barHeight
  in pictures [woodPattern, rivets, border]

-- ============================================================================
-- Treasury Panel - Wooden Panel with Gold Coin Icon
-- ============================================================================

renderTreasuryPanel :: World -> Picture
renderTreasuryPanel world =
  let gold = resGold (resources world)
      panelX = -worldWidth/2 + 120
      panelY = worldHeight/2 - 80
      panelWidth = 180
      panelHeight = 50
      -- Wooden panel background
      panelBg = translate panelX panelY $ pictures
        [ color woodBrown $ rectangleSolid panelWidth panelHeight
        , color darkWoodBrown $ rectangleWire panelWidth panelHeight
        ]
      -- Gold coin icon (8x8 pixel art)
      coinIcon = translate (panelX - 60) panelY $ renderGoldCoinIcon
      -- Gold text
      goldText = translate (panelX + 20) panelY $ color goldColor $ scale 0.15 0.15 $ text (show gold)
  in pictures [panelBg, coinIcon, goldText]

-- Gold coin icon (8x8 pixel art)
renderGoldCoinIcon :: Picture
renderGoldCoinIcon =
  let pixelSize = 4
      pixels = [ (-1, -1, "gold"), (0, -1, "gold"), (1, -1, "gold")
               , (-1, 0, "gold"), (0, 0, "yellow"), (1, 0, "gold")
               , (-1, 1, "gold"), (0, 1, "gold"), (1, 1, "gold")
               ]
      gold = makeColor 1.0 0.84 0.0 1.0
      yellow = makeColor 1.0 1.0 0.0 1.0
  in pictures $ map (\(x, y, col) ->
    translate (x * pixelSize) (y * pixelSize) $
    color (if col == "gold" then gold else yellow) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- ============================================================================
-- Level Panel - Parchment Rectangle with LEVEL and WAVE
-- ============================================================================

renderLevelPanel :: World -> Picture
renderLevelPanel world =
  let ws = waveState world
      level = wsLevel ws
      wave = wsWaveInLevel ws
      panelX = -worldWidth/2 + 120
      panelY = worldHeight/2 - 140
      panelWidth = 200
      panelHeight = 60
      -- Parchment background
      parchmentBg = translate panelX panelY $ pictures
        [ color parchment $ rectangleSolid panelWidth panelHeight
        , color darkWoodBrown $ rectangleWire panelWidth panelHeight
        ]
      -- Level icon (small shield)
      levelIcon = translate (panelX - 70) panelY $ renderShieldIcon
      -- Wave icon (pixel water wave)
      waveIcon = translate (panelX - 70) (panelY - 25) $ renderWaveIcon
      -- Text
      levelText = translate (panelX - 20) (panelY + 10) $ color (makeColor 0.2 0.2 0.2 1) $ scale 0.12 0.12 $ text ("LEVEL " ++ show level)
      waveText = translate (panelX - 20) (panelY - 15) $ color (makeColor 0.2 0.2 0.2 1) $ scale 0.12 0.12 $ text ("WAVE " ++ show wave)
  in pictures [parchmentBg, levelIcon, waveIcon, levelText, waveText]

-- Shield icon (8x8 pixel art)
renderShieldIcon :: Picture
renderShieldIcon =
  let pixelSize = 4
      pixels = [ (0, -1, "silver")
               , (-1, 0, "silver"), (0, 0, "blue"), (1, 0, "silver")
               , (-1, 1, "silver"), (0, 1, "blue"), (1, 1, "silver")
               , (0, 2, "silver")
               ]
  in pictures $ map (\(x, y, col) ->
    translate (x * pixelSize) (y * pixelSize) $
    color (pixelColor col) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- Wave icon (8x8 pixel art)
renderWaveIcon :: Picture
renderWaveIcon =
  let pixelSize = 4
      pixels = [ (-1, 0, "blue"), (0, 0, "light blue"), (1, 0, "blue")
               , (-1, 1, "light blue"), (0, 1, "blue"), (1, 1, "light blue")
               ]
  in pictures $ map (\(x, y, col) ->
    translate (x * pixelSize) (y * pixelSize) $
    color (pixelColor col) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- ============================================================================
-- Castle HP Bar - Stone-Bordered Health Bar
-- ============================================================================

renderCastleHPBar :: World -> Picture
renderCastleHPBar world =
  let c = castle world
      currentHP = castleHP c
      maxHP = Types.castleMaxHP c
      ratio = currentHP / maxHP
      barX = worldWidth/2 - 200
      barY = worldHeight/2 - 80
      barWidth = 300  -- Reduced from 400
      barHeight = 20  -- Reduced from 30
      -- Determine color based on health state
      (barColor, state) = if ratio > 0.7 then (makeColor 0.2 0.8 0.2 1, "healthy")
                         else if ratio > 0.3 then (makeColor 1.0 0.8 0.0 1, "damaged")
                         else (makeColor 0.8 0.2 0.2 1, "critical")
      -- Stone border
      stoneBorder = translate barX barY $ pictures
        [ color darkStoneGray $ rectangleSolid (barWidth + 6) (barHeight + 6)
        , color stoneGray $ rectangleSolid barWidth barHeight
        ]
      -- Health fill
      healthFill = translate (barX - (barWidth * (1 - ratio) / 2)) barY $
        color barColor $ rectangleSolid (barWidth * ratio) barHeight
      -- Text
      hpText = translate (barX - 140) barY $ color (makeColor 1.0 1.0 1.0 1) $ scale 0.10 0.10 $ text "CASTLE"
      hpValue = translate (barX + 160) barY $ color (makeColor 1.0 1.0 1.0 1) $ scale 0.09 0.09 $ text (show (round currentHP) ++ "/" ++ show (round maxHP))
  in pictures [stoneBorder, healthFill, hpText, hpValue]

-- ============================================================================
-- Tower Hotbar - Wooden Framed Button Icons
-- ============================================================================

renderTowerHotbar :: World -> Picture
renderTowerHotbar world =
  let hotbarX = -worldWidth/2 + 120
      hotbarY = worldHeight/2 - 220
      towerTypes = [ArrowTower, BallistaTower, FireTower, TeslaTower, BombardTower]
      towerIcons = zipWith (\idx tt -> renderTowerButton (hotbarX + fromIntegral idx * 70) hotbarY tt (inputState world)) [0..] towerTypes
  in pictures towerIcons

renderTowerButton :: Float -> Float -> TowerType -> InputState -> Picture
renderTowerButton x y towerType inputState =
  let buttonWidth = 60
      buttonHeight = 60
      isSelected = case buildMode inputState of
                    PlaceTower tt -> tt == towerType
                    _ -> False
      -- Wooden frame
      frame = translate x y $ pictures
        [ color (if isSelected then darkWoodBrown else woodBrown) $ rectangleSolid buttonWidth buttonHeight
        , color darkWoodBrown $ rectangleWire buttonWidth buttonHeight
        ]
      -- Tower icon (16x16 pixel art)
      icon = translate x y $ renderTowerIcon towerType
      -- Key label
      keyLabel = case towerType of
                  ArrowTower -> "4"
                  BallistaTower -> "5"
                  FireTower -> "6"
                  TeslaTower -> "7"
                  BombardTower -> "8"
                  _ -> ""
      keyText = translate (x - 20) (y - 30) $ color (makeColor 1.0 1.0 1.0 1) $ scale 0.1 0.1 $ text keyLabel
  in pictures [frame, icon, keyText]

-- Tower icons (16x16 pixel art simplified)
renderTowerIcon :: TowerType -> Picture
renderTowerIcon tt =
  let pixelSize = 3
      pixels = case tt of
                ArrowTower -> [(-2, -2, "brown"), (0, -2, "brown"), (2, -2, "brown"), (0, 0, "tan"), (0, 2, "brown")]
                BallistaTower -> [(-1, -2, "brown"), (0, -2, "silver"), (1, -2, "brown"), (0, 0, "brown"), (0, 2, "brown")]
                FireTower -> [(-1, -2, "dark gray"), (0, -2, "red"), (1, -2, "dark gray"), (0, 0, "orange"), (0, 2, "red")]
                TeslaTower -> [(-1, -2, "gray"), (0, -2, "blue"), (1, -2, "gray"), (0, 0, "white"), (0, 2, "blue")]
                BombardTower -> [(-2, -1, "gray"), (0, -1, "black"), (2, -1, "gray"), (0, 1, "black")]
                _ -> []
  in pictures $ map (\(px, py, col) ->
    translate (px * pixelSize) (py * pixelSize) $
    color (pixelColor col) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- ============================================================================
-- Trap Hotbar - Wooden Framed Buttons for Traps
-- ============================================================================

renderTrapHotbar :: World -> Picture
renderTrapHotbar world =
  let hotbarX = -worldWidth/2 + 120
      hotbarY = worldHeight/2 - 300
      trapTypes = [SpikeTrap, FreezeTrap, FirePitTrap, MagicSnareTrap, ExplosiveBarrel]
      trapIcons = zipWith (\idx tt -> renderTrapButton (hotbarX + fromIntegral idx * 70) hotbarY tt (inputState world)) [0..] trapTypes
  in pictures trapIcons

renderTrapButton :: Float -> Float -> TrapType -> InputState -> Picture
renderTrapButton x y trapType inputState =
  let buttonWidth = 60
      buttonHeight = 60
      isSelected = case buildMode inputState of
                    PlaceTrap tt -> tt == trapType
                    _ -> False
      -- Wooden frame
      frame = translate x y $ pictures
        [ color (if isSelected then darkWoodBrown else woodBrown) $ rectangleSolid buttonWidth buttonHeight
        , color darkWoodBrown $ rectangleWire buttonWidth buttonHeight
        ]
      -- Trap icon (16x16 pixel art)
      icon = translate x y $ renderTrapIcon trapType
      -- Key label
      keyLabel = case trapType of
                  SpikeTrap -> "Z"
                  FreezeTrap -> "X"
                  FirePitTrap -> "C"
                  MagicSnareTrap -> "V"
                  ExplosiveBarrel -> "B"
      keyText = translate (x - 20) (y - 30) $ color (makeColor 1.0 1.0 1.0 1) $ scale 0.1 0.1 $ text keyLabel
  in pictures [frame, icon, keyText]

-- Trap icons (16x16 pixel art simplified)
renderTrapIcon :: TrapType -> Picture
renderTrapIcon tt =
  let pixelSize = 3
      pixels = case tt of
                SpikeTrap -> [(-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (-1, 0, "dark gray"), (0, 0, "silver"), (1, 0, "dark gray"), (-1, 1, "gray"), (0, 1, "dark gray"), (1, 1, "gray")]
                FreezeTrap -> [(-1, -1, "blue"), (0, -1, "light blue"), (1, -1, "blue"), (-1, 0, "light blue"), (0, 0, "white"), (1, 0, "light blue"), (-1, 1, "blue"), (0, 1, "light blue"), (1, 1, "blue")]
                FirePitTrap -> [(-1, -1, "dark brown"), (0, -1, "orange"), (1, -1, "dark brown"), (-1, 0, "orange"), (0, 0, "yellow"), (1, 0, "orange"), (-1, 1, "dark brown"), (0, 1, "orange"), (1, 1, "dark brown")]
                MagicSnareTrap -> [(-1, -1, "purple"), (0, -1, "magenta"), (1, -1, "purple"), (-1, 0, "magenta"), (0, 0, "purple"), (1, 0, "magenta"), (-1, 1, "purple"), (0, 1, "magenta"), (1, 1, "purple")]
                ExplosiveBarrel -> [(-1, -1, "brown"), (0, -1, "dark brown"), (1, -1, "brown"), (-1, 0, "dark brown"), (0, 0, "red"), (1, 0, "dark brown"), (-1, 1, "brown"), (0, 1, "dark brown"), (1, 1, "brown"), (0, 2, "red")]
                _ -> []
  in pictures $ map (\(px, py, col) ->
    translate (px * pixelSize) (py * pixelSize) $
    color (pixelColor col) $
    rectangleSolid pixelSize pixelSize
    ) pixels

-- ============================================================================
-- Build Mode Display
-- ============================================================================

renderBuildModeDisplay :: World -> Picture
renderBuildModeDisplay world =
  case buildMode (inputState world) of
    NoBuild -> blank
    PlaceTower tt -> 
      let goldAvail = resGold (resources world)
          cost = C.towerCost tt
          canAfford = goldAvail >= cost
          panelX = worldWidth/2 - 200
          panelY = -worldHeight/2 + 100
          -- Parchment tooltip panel
          tooltip = translate panelX panelY $ pictures
            [ color parchment $ rectangleSolid 300 80
            , color darkWoodBrown $ rectangleWire 300 80
            , translate (panelX - 120) panelY $ color (if canAfford then (makeColor 0.2 0.6 0.2 1) else (makeColor 0.8 0.2 0.2 1)) $ scale 0.12 0.12 $ text $ "TOWER: " ++ show tt ++ " (" ++ show cost ++ "g)"
            ]
      in tooltip
    PlaceTrap tt -> 
      let goldAvail = resGold (resources world)
          cost = C.trapCost tt
          canAfford = goldAvail >= cost
          panelX = worldWidth/2 - 200
          panelY = -worldHeight/2 + 100
          -- Parchment tooltip panel
          tooltip = translate panelX panelY $ pictures
            [ color parchment $ rectangleSolid 300 80
            , color darkWoodBrown $ rectangleWire 300 80
            , translate (panelX - 120) panelY $ color (if canAfford then (makeColor 0.2 0.6 0.2 1) else (makeColor 0.8 0.2 0.2 1)) $ scale 0.12 0.12 $ text $ "TRAP: " ++ show tt ++ " (" ++ show cost ++ "g)"
            ]
      in tooltip
    UpgradeMode -> 
      let panelX = worldWidth/2 - 200
          panelY = -worldHeight/2 + 100
          tooltip = translate panelX panelY $ pictures
            [ color parchment $ rectangleSolid 250 60
            , color darkWoodBrown $ rectangleWire 250 60
            , translate (panelX - 100) panelY $ color (makeColor 0.8 0.6 0.0 1) $ scale 0.12 0.12 $ text "UPGRADE MODE"
            ]
      in tooltip

-- ============================================================================
-- Game Status
-- ============================================================================

renderGameStatus :: World -> Picture
renderGameStatus world
  | isGameOver world && isVictory world =
      let panelX = 0
          panelY = 0
      in translate panelX panelY $ pictures
        [ color parchment $ rectangleSolid 400 150
        , color darkWoodBrown $ rectangleWire 400 150
        , translate (panelX - 150) panelY $ color (makeColor 0.2 0.8 0.2 1) $ scale 0.3 0.3 $ text "VICTORY"
        ]
  | isGameOver world =
      let panelX = 0
          panelY = 0
      in translate panelX panelY $ pictures
        [ color parchment $ rectangleSolid 400 150
        , color darkWoodBrown $ rectangleWire 400 150
        , translate (panelX - 120) panelY $ color (makeColor 0.8 0.2 0.2 1) $ scale 0.3 0.3 $ text "DEFEAT"
        ]
  | isPaused world =
      let panelX = 0
          panelY = 0
      in translate panelX panelY $ pictures
        [ color parchment $ rectangleSolid 300 100
        , color darkWoodBrown $ rectangleWire 300 100
        , translate (panelX - 100) panelY $ color (makeColor 0.8 0.6 0.0 1) $ scale 0.2 0.2 $ text "PAUSED"
        ]
  | otherwise = blank
