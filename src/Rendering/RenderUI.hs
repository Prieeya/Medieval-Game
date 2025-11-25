module Rendering.RenderUI where

import Graphics.Gloss
import Types
import Constants
import qualified Data.Map.Strict as M

-- ============================================================================
-- UI Rendering - Medieval Theme
-- ============================================================================

renderUI :: World -> Picture
renderUI world = translate (-worldWidth/2 + 20) (worldHeight/2 - 30) $ pictures
  [ renderResourceDisplay world
  , translate 0 (-60) $ renderWaveDisplay world
  , translate 0 (-120) $ renderAbilityDisplay world
  , translate 0 (-200) $ renderBuildModeDisplay world
  , translate 0 (-260) $ renderGameStatus world
  , translate 0 (-320) $ renderUpgradeInfo world
  ]

-- ============================================================================
-- Resource Display - Medieval Style
-- ============================================================================

renderResourceDisplay :: World -> Picture
renderResourceDisplay world =
  let gold = resGold (resources world)
      goldColor = makeColor 1.0 0.84 0.0 1.0  -- Golden yellow
      shadowColor = makeColor 0.0 0.0 0.0 0.8  -- Black shadow for readability
      -- Add shadow for better visibility
      renderTextWithShadow txt x y scale' col =
        pictures
          [ translate (x + 1) (y - 1) $ color shadowColor $ scale scale' scale' $ text txt
          , translate x y $ color col $ scale scale' scale' $ text txt
          ]
  in pictures
    [ renderTextWithShadow "⚔ TREASURY ⚔" 0 0 0.18 goldColor
    , renderTextWithShadow ("Coins: " ++ show gold) 0 (-25) 0.16 goldColor
    ]

-- ============================================================================
-- Wave Display - Medieval Theme
-- ============================================================================

renderWaveDisplay :: World -> Picture
renderWaveDisplay world =
  let ws = waveState world
      level = wsLevel ws
      wave = wsWaveInLevel ws
      phase = wsPhase ws
      phaseText = case phase of
                    InWave -> "⚔ BATTLE ⚔"
                    BuildPhase t -> "Prepare: " ++ show (round t :: Int) ++ "s"
                    BossIncoming t -> "👑 BOSS INCOMING: " ++ show (round t :: Int) ++ "s"
      phaseColor = case phase of
                     InWave -> makeColor 1.0 0.2 0.2 1.0  -- Red for battle
                     BuildPhase _ -> makeColor 0.2 1.0 0.2 1.0  -- Green for build
                     BossIncoming _ -> makeColor 1.0 0.5 0.0 1.0  -- Orange for boss
      shadowColor = makeColor 0.0 0.0 0.0 0.8
      renderTextWithShadow txt x y scale' col =
        pictures
          [ translate (x + 1) (y - 1) $ color shadowColor $ scale scale' scale' $ text txt
          , translate x y $ color col $ scale scale' scale' $ text txt
          ]
  in pictures
    [ renderTextWithShadow ("LEVEL " ++ show level ++ " - WAVE " ++ show wave) 0 0 0.16 (makeColor 1.0 1.0 1.0 1.0)
    , renderTextWithShadow phaseText 0 (-22) 0.14 phaseColor
    ]

-- ============================================================================
-- Ability Display - Medieval Theme
-- ============================================================================

renderAbilityDisplay :: World -> Picture
renderAbilityDisplay world =
  let abilities' = M.elems (abilities world)
      renders = zipWith renderAbility [0..] abilities'
  in pictures renders

renderAbility :: Int -> AbilityState -> Picture
renderAbility idx ability =
  let x = fromIntegral idx * 70
      cooldown = abilityCooldown ability
      active = abilityActive ability
      abilityColor = if active then makeColor 0.2 1.0 0.2 1.0
                     else if cooldown > 0 then makeColor 0.8 0.2 0.2 1.0
                     else makeColor 0.8 0.8 0.8 1.0
      name = case abilityType ability of
               Firestorm -> "Q: FIRE"
               FreezeField -> "W: FROST"
               RepairWalls -> "E: REPAIR"
               TimeSlow -> "R: CHRONO"
      borderColor = if cooldown > 0 then makeColor 0.5 0 0 1.0 else makeColor 0.5 0.5 0.5 1.0
  in translate x 0 $ pictures
    [ color borderColor $ rectangleWire 50 40
    , color abilityColor $ scale 0.09 0.09 $ text name
    , if cooldown > 0
      then translate 0 (-18) $ color (makeColor 1.0 0.2 0.2 1.0) $ scale 0.08 0.08 $ text (show (round cooldown) ++ "s")
      else blank
    ]

-- ============================================================================
-- Build Mode Display - Medieval Theme
-- ============================================================================

renderBuildModeDisplay :: World -> Picture
renderBuildModeDisplay world =
  case buildMode (inputState world) of
    NoBuild -> blank
    PlaceTower tt -> 
      let goldAvail = resGold (resources world)
          cost = towerCost tt
          canAfford = goldAvail >= cost
          color' = if canAfford then makeColor 0.2 1.0 0.2 1.0 else makeColor 1.0 0.2 0.2 1.0
      in pictures
        [ color color' $ scale 0.13 0.13 $ text $ "TOWER: " ++ show tt ++ " (" ++ show cost ++ "g)"
        ]
    PlaceTrap tt -> 
      let goldAvail = resGold (resources world)
          cost = trapCost tt
          canAfford = goldAvail >= cost
          color' = if canAfford then makeColor 1.0 0.6 0.2 1.0 else makeColor 1.0 0.2 0.2 1.0
      in pictures
        [ color color' $ scale 0.13 0.13 $ text $ "TRAP: " ++ show tt ++ " (" ++ show cost ++ "g)"
        ]
    UpgradeMode -> color (makeColor 1.0 0.8 0.0 1.0) $ scale 0.13 0.13 $ text "UPGRADE MODE"

-- ============================================================================
-- Game Status - Medieval Theme
-- ============================================================================

renderGameStatus :: World -> Picture
renderGameStatus world
  | isGameOver world && isVictory world =
      color (makeColor 0.2 1.0 0.2 1.0) $ scale 0.25 0.25 $ text "⚔ VICTORY ⚔"
  | isGameOver world =
      color (makeColor 1.0 0.2 0.2 1.0) $ scale 0.25 0.25 $ text "☠ DEFEAT ☠"
  | isPaused world =
      color (makeColor 1.0 0.8 0.0 1.0) $ scale 0.18 0.18 $ text "⏸ PAUSED ⏸"
  | otherwise = blank

-- ============================================================================
-- Upgrade Info Display
-- ============================================================================

renderUpgradeInfo :: World -> Picture
renderUpgradeInfo world =
  let level = wsLevel (waveState world)
      upgradeUnlockLevel = 3
      canUpgrade = level >= upgradeUnlockLevel
      color' = if canUpgrade then makeColor 0.2 1.0 0.2 1.0 else makeColor 0.5 0.5 0.5 1.0
  in pictures
    [ if canUpgrade
      then color color' $ scale 0.12 0.12 $ text "UPGRADES AVAILABLE - Press U to Upgrade Towers"
      else color color' $ scale 0.11 0.11 $ text $ "Upgrades available at Level " ++ show upgradeUnlockLevel
    ]