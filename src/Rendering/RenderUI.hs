module Rendering.RenderUI where

import Graphics.Gloss
import Types
import Constants hiding (castleMaxHP)
import qualified Constants as C
import qualified Data.Map.Strict as M
import Rendering.PixelArt (pixelColor)

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
  -- Removed renderBuildModeDisplay - was showing empty white box
  -- , renderBuildModeDisplay world
  , renderGameStatus world
  , renderWaveTimer world
  , renderGateRepairPrompt world
  , renderQuitResetButtons world
  , renderDialog world
  , renderShop world
  , renderHelpMenu world
  , renderGameMessage world
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
      maxHP = castleMaxHP c
      ratio = currentHP / maxHP
      barX = worldWidth/2 - 200
      barY = worldHeight/2 - 80
      barWidth = 150  -- Castle HP bar width
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
-- Wave Timer Display
-- ============================================================================

renderWaveTimer :: World -> Picture
renderWaveTimer world =
  case wsPhase (waveState world) of
    BuildPhase timeLeft ->
      let timerX = 0
          timerY = 200  -- Center-top of screen
          timeRemaining = max 0 timeLeft
          seconds = ceiling timeRemaining
          -- Build phase timer display
          timerPanel = translate timerX timerY $ pictures
            [ color (makeColor 0.1 0.1 0.1 0.8) $ rectangleSolid 350 120
            , color darkWoodBrown $ rectangleWire 350 120
            , translate 0 30 $ 
                color (makeColor 0.8 0.8 1.0 1) $ scale 0.15 0.15 $ text "BUILD PHASE"
            , translate 0 (-30) $ 
                color (if seconds <= 5 then (makeColor 1.0 0.6 0.0 1) else (makeColor 0.6 0.8 1.0 1)) $ 
                scale 0.5 0.5 $ text (show seconds)
            ]
      in timerPanel
    WaveCountdown timeLeft ->
      let timerX = 0
          timerY = 200  -- Center-top of screen
          timeRemaining = max 0 timeLeft
          seconds = ceiling timeRemaining
          -- Large, prominent timer display
          timerPanel = translate timerX timerY $ pictures
            [ color (makeColor 0.1 0.1 0.1 0.8) $ rectangleSolid 300 120
            , color darkWoodBrown $ rectangleWire 300 120
            , translate 0 30 $ 
                color (makeColor 1.0 1.0 1.0 1) $ scale 0.15 0.15 $ text "WAVE INCOMING"
            , translate 0 (-30) $ 
                color (if seconds <= 2 then (makeColor 1.0 0.2 0.2 1) else (makeColor 1.0 0.9 0.0 1)) $ 
                scale 0.5 0.5 $ text (show seconds)
            ]
      in timerPanel
    BossIncoming timeLeft ->
      let timerX = 0
          timerY = 200  -- Center-top of screen
          timeRemaining = max 0 timeLeft
          seconds = ceiling timeRemaining
          -- Boss incoming timer display
          timerPanel = translate timerX timerY $ pictures
            [ color (makeColor 0.2 0.1 0.1 0.8) $ rectangleSolid 350 120
            , color (makeColor 0.8 0.2 0.2 1) $ rectangleWire 350 120
            , translate 0 30 $ 
                color (makeColor 1.0 0.3 0.3 1) $ scale 0.15 0.15 $ text "BOSS INCOMING"
            , translate 0 (-30) $ 
                color (if seconds <= 5 then (makeColor 1.0 0.2 0.2 1) else (makeColor 1.0 0.5 0.5 1)) $ 
                scale 0.5 0.5 $ text (show seconds)
            ]
      in timerPanel
    _ -> blank

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

-- ============================================================================
-- Gate Repair Prompt
-- ============================================================================

renderGateRepairPrompt :: World -> Picture
renderGateRepairPrompt world =
  let ws = waveState world
      repairPending = wsGateRepairPending ws
      gates = fortGates (fort world)
      anyGateDestroyed = any gateDestroyed gates
      panelX = 0
      panelY = -worldHeight/2 + 50  -- Bottom of screen
      panelWidth = 550  -- Wider box to fit text
      panelHeight = 60
  in if repairPending && anyGateDestroyed
     then
       translate panelX panelY $ pictures
         [ -- White box background
           color (makeColor 1.0 1.0 1.0 1.0) $ rectangleSolid panelWidth panelHeight
         , -- Black border
           color (makeColor 0.0 0.0 0.0 1.0) $ rectangleWire panelWidth panelHeight
         , -- Black text (centered inside the box)
           -- Approximate centering: shift left by half the estimated text width
           -- Text is ~40 chars, at scale 0.13 each char ~5px = ~200px total, so shift -100px to center
           translate (-100) 5 $ 
             color (makeColor 0.0 0.0 0.0 1.0) $ 
             scale 0.13 0.13 $ 
             text "Press G to repair the gate for 50 gold"
         ]
     else blank

-- ============================================================================
-- Quit/Reset Buttons
-- ============================================================================

renderQuitResetButtons :: World -> Picture
renderQuitResetButtons world =
  let buttonY = worldHeight/2 - 30
      quitX = worldWidth/2 - 80
      resetX = worldWidth/2 - 80
  in pictures
    [ -- Quit Button
      translate quitX (buttonY - 30) $ pictures
        [ color (makeColor 0.8 0.2 0.2 1) $ rectangleSolid 60 25
        , color darkWoodBrown $ rectangleWire 60 25
        , translate (-20) (-5) $ scale 0.1 0.1 $ color (makeColor 1 1 1 1) $ text "QUIT"
        ]
    , -- Reset Button
      translate resetX (buttonY - 60) $ pictures
        [ color (makeColor 0.2 0.6 0.8 1) $ rectangleSolid 60 25
        , color darkWoodBrown $ rectangleWire 60 25
        , translate (-20) (-5) $ scale 0.1 0.1 $ color (makeColor 1 1 1 1) $ text "RESET"
        ]
    ]

-- ============================================================================
-- Dialogs & Menus
-- ============================================================================

renderDialog :: World -> Picture
renderDialog world =
  case buildMode (inputState world) of
    ConfirmationDialog msg _ _ ->
      let panelW = 400
          panelH = 200
      in pictures
        [ -- Dim background
          color (makeColor 0 0 0 0.5) $ rectangleSolid worldWidth worldHeight
        , -- Dialog Box
          pictures
            [ color parchment $ rectangleSolid panelW panelH
            , color darkWoodBrown $ rectangleWire panelW panelH
            , -- Message
              translate (-150) 40 $ scale 0.2 0.2 $ color (makeColor 0 0 0 1) $ text msg
            , -- Confirm Button (Green)
              translate (-60) (-40) $ pictures
                [ color (makeColor 0.2 0.8 0.2 1) $ rectangleSolid 80 40
                , color darkWoodBrown $ rectangleWire 80 40
                , translate (-25) (-5) $ scale 0.12 0.12 $ color (makeColor 1 1 1 1) $ text "YES"
                ]
            , -- Cancel Button (Red)
              translate 60 (-40) $ pictures
                [ color (makeColor 0.8 0.2 0.2 1) $ rectangleSolid 80 40
                , color darkWoodBrown $ rectangleWire 80 40
                , translate (-20) (-5) $ scale 0.12 0.12 $ color (makeColor 1 1 1 1) $ text "NO"
                ]
            ]
        ]
    _ -> blank

renderShop :: World -> Picture
renderShop world =
  case buildMode (inputState world) of
    ShopMenu ->
      let panelW = 600
          panelH = 500
          gold = resGold (resources world)
      in pictures
        [ -- Dim background
          color (makeColor 0 0 0 0.5) $ rectangleSolid worldWidth worldHeight
        , -- Shop Panel
          pictures
            [ color parchment $ rectangleSolid panelW panelH
            , color darkWoodBrown $ rectangleWire panelW panelH
            , -- Header
              translate (-100) 200 $ scale 0.3 0.3 $ color darkWoodBrown $ text "SHOP"
            , -- Close Button
              translate (panelW/2 - 30) (panelH/2 - 30) $ color (makeColor 0.8 0.2 0.2 1) $ rectangleSolid 40 40
            , translate (panelW/2 - 40) (panelH/2 - 40) $ scale 0.2 0.2 $ color (makeColor 1 1 1 1) $ text "X"
            , -- Content
              translate (-250) 100 $ scale 0.15 0.15 $ color (makeColor 0 0 0 1) $ text "UPGRADES AVAILABLE:"
            , translate (-250) 50 $ scale 0.15 0.15 $ color (makeColor 0 0 0 1) $ text "- New Towers (Level 5)"
            , translate (-250) 0 $ scale 0.15 0.15 $ color (makeColor 0 0 0 1) $ text "- New Traps (Level 3)"
            , translate (-250) (-50) $ scale 0.15 0.15 $ color (makeColor 0 0 0 1) $ text ("GOLD: " ++ show gold)
            
            , -- Unlock Button
              if not (upgradeUnlocked $ upgradeUnlock world)
              then translate 0 (-150) $ pictures
                [ color (if gold >= 100 then makeColor 0.2 0.8 0.2 1 else makeColor 0.5 0.5 0.5 1) $ rectangleSolid 250 50
                , color darkWoodBrown $ rectangleWire 250 50
                , translate (-110) (-10) $ scale 0.15 0.15 $ color (makeColor 1 1 1 1) $ text "UNLOCK ALL (100g)"
                ]
              else translate 0 (-150) $ pictures
                [ color (makeColor 0.2 0.6 0.2 1) $ rectangleSolid 250 50
                , translate (-80) (-10) $ scale 0.15 0.15 $ color (makeColor 1 1 1 1) $ text "UNLOCKED!"
                ]
            ]
        ]
    _ -> blank

renderHelpMenu :: World -> Picture
renderHelpMenu world =
  case buildMode (inputState world) of
    HelpMenu ->
      let panelW = 750
          panelH = 650
      in pictures
        [ -- Dim background
          color (makeColor 0 0 0 0.7) $ rectangleSolid worldWidth worldHeight
        , -- Help Panel
          pictures
            [ color parchment $ rectangleSolid panelW panelH
            , color darkWoodBrown $ rectangleWire panelW panelH
            , -- Header
              translate (-150) 280 $ scale 0.3 0.3 $ color darkWoodBrown $ text "GAME LEGEND & UPGRADE GUIDE"
            , -- Close Button
              translate (panelW/2 - 30) (panelH/2 - 30) $ color (makeColor 0.8 0.2 0.2 1) $ rectangleSolid 40 40
            , translate (panelW/2 - 40) (panelH/2 - 40) $ scale 0.2 0.2 $ color (makeColor 1 1 1 1) $ text "X"
            -- LEFT COLUMN
            , translate (-320) 240 $ scale 0.12 0.12 $ color (makeColor 0.8 0.5 0.1 1) $ text "UPGRADE SYSTEM:"
            , translate (-320) 220 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "U key + click tower/trap to upgrade (max Lv3)"
            , translate (-320) 200 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "H key - Upgrade all gates (max Lv5)"
            , translate (-320) 180 $ scale 0.09 0.09 $ color (makeColor 0.1 0.6 0.1 1) $ text "Lv2: +25% stats, Lv3: +50% + Special Ability"
            , translate (-320) 150 $ scale 0.12 0.12 $ color (makeColor 0 0 0 1) $ text "TOWERS (4-0 keys):"
            , translate (-320) 130 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Arrow(50g) Catapult(120g) Crossbow(150g)"
            , translate (-320) 110 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Fire(130g) Tesla(200g) Ballista(220g)"
            , translate (-320) 90 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Poison(90g) Bombard(250g)"
            , translate (-320) 60 $ scale 0.12 0.12 $ color (makeColor 0 0 0 1) $ text "TRAPS (Z,X,C,V,B keys):"
            , translate (-320) 40 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Z-Spike(20g) X-Freeze(35g) C-FirePit(45g)"
            , translate (-320) 20 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "V-MagicSnare(50g) B-Explosive(80g)"
            , translate (-320) (-10) $ scale 0.12 0.12 $ color (makeColor 0 0 0 1) $ text "ENEMIES:"
            , translate (-320) (-30) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "Grunt - Basic | Shieldbearer - Armored"
            , translate (-320) (-48) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "Direwolf - Fast | Pyromancer - Ranged"
            , translate (-320) (-66) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "BruteCrusher - Heavy | TrapBreaker - Anti-trap"
            , translate (-320) (-84) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "WallClimber - Bypasses gate"
            , translate (-320) (-102) $ scale 0.08 0.08 $ color (makeColor 0.8 0.3 0.1 1) $ text "Berserker - High dmg melee, climbs"
            , translate (-320) (-120) $ scale 0.08 0.08 $ color (makeColor 0.4 0.2 0.5 1) $ text "Assassin - Fast, targets towers"
            , translate (-320) (-138) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "BoulderRam - Siege unit"
            , translate (-320) (-170) $ scale 0.12 0.12 $ color (makeColor 0.8 0.2 0.2 1) $ text "BOSSES (Level 3):"
            , translate (-320) (-190) $ scale 0.08 0.08 $ color (makeColor 0.6 0.1 0.1 1) $ text "Ironback Minotaur - Tank, charge attack"
            , translate (-320) (-208) $ scale 0.08 0.08 $ color (makeColor 0.8 0.3 0.1 1) $ text "Fire Drake - Fire breath AoE"
            , translate (-320) (-226) $ scale 0.08 0.08 $ color (makeColor 0.3 0.1 0.5 1) $ text "Lich King - Summons undead"
            -- RIGHT COLUMN
            , translate 50 240 $ scale 0.12 0.12 $ color (makeColor 0 0 0 1) $ text "CONTROLS:"
            , translate 50 220 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "TAB - Shop Menu  M - This Help"
            , translate 50 200 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "H - Upgrade Gates  G - Repair Gates"
            , translate 50 180 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "U - Upgrade Mode (click tower/trap)"
            , translate 50 160 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Space - Pause  1/2/3 - Speed"
            , translate 50 130 $ scale 0.12 0.12 $ color (makeColor 0.1 0.5 0.8 1) $ text "LEVEL PROGRESSION:"
            , translate 50 110 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Level 1: Basic enemies"
            , translate 50 90 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Level 2: +Climbers, Berserkers"
            , translate 50 70 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Level 3: All enemies + BOSS WAVE"
            , translate 50 50 $ scale 0.09 0.09 $ color (makeColor 0.1 0.7 0.1 1) $ text "Victory after Level 3!"
            , translate 50 20 $ scale 0.12 0.12 $ color (makeColor 0.8 0.5 0.1 1) $ text "UPGRADE COSTS:"
            , translate 50 0 $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Towers: Base/2 + 50g per level"
            , translate 50 (-20) $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Traps: Base/2 + 25g per level"
            , translate 50 (-40) $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Gates: 150g + 100g per level"
            , translate 50 (-70) $ scale 0.12 0.12 $ color (makeColor 0.8 0.2 0.2 1) $ text "PRIORITY TARGETS:"
            , translate 50 (-90) $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Archers prioritize climbers!"
            , translate 50 (-110) $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Siege towers target inside fort"
            , translate 50 (-130) $ scale 0.09 0.09 $ color (makeColor 0.2 0.2 0.2 1) $ text "Traps hidden until enemy nearby"
            , translate 50 (-160) $ scale 0.12 0.12 $ color (makeColor 0 0 0 1) $ text "TIPS:"
            , translate 50 (-180) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "Upgrade towers before new waves"
            , translate 50 (-198) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "Place traps on enemy paths"
            , translate 50 (-216) $ scale 0.08 0.08 $ color (makeColor 0.2 0.2 0.2 1) $ text "Archers near walls for climbers"
            ]
        ]
    _ -> blank

renderGameMessage :: World -> Picture
renderGameMessage world =
  case gameMessage world of
    Nothing -> blank
    Just (msg, timeLeft) ->
      let alpha = min 1.0 (timeLeft / 0.5)  -- Fade out in last 0.5 seconds
          msgColor = makeColor 1.0 0.9 0.2 (alpha * 0.9)  -- Gold/yellow color
          bgColor = makeColor 0 0 0 (alpha * 0.7)  -- Dark background
      in translate 0 (worldHeight/2 - 100) $ pictures
        [ color bgColor $ rectangleSolid 600 60
        , color darkWoodBrown $ rectangleWire 600 60
        , translate (-250) (-5) $ scale 0.15 0.15 $ color msgColor $ text msg
        ]
