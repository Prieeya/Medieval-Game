module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Types
import Config
import GameLoop
import Input.InputHandler
import Rendering.RenderWorld
import Rendering.RenderUI
import Rendering.RenderDebug

-- ============================================================================
-- Main Entry Point
-- ============================================================================

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "Medieval Siege Defense"
  putStrLn "==================================="
  putStrLn ""
  putStrLn "Controls:"
  putStrLn "  4-0: Select tower type to build"
  putStrLn "  Z-B: Select trap type to place"
  putStrLn "  U: Upgrade mode"
  putStrLn "  ESC: Cancel build mode"
  putStrLn "  Q/W/E/R: Activate abilities"
  putStrLn "  Space: Pause/Unpause"
  putStrLn "  1/2/3: Game speed (1x/2x/4x)"
  putStrLn "  F1: Toggle debug display"
  putStrLn ""
  putStrLn "Objective:"
  putStrLn "  Defend your castle from waves of enemies!"
  putStrLn "  Place towers INSIDE the fort."
  putStrLn "  Place traps ANYWHERE."
  putStrLn "  Survive 5 levels to win!"
  putStrLn ""
  putStrLn "Starting game..."
  putStrLn ""
  
  let fps = 60
      world0 = initialWorld
  
  play
    FullScreen
    black
    fps
    world0
    render
    handleInput
    updateWorld

-- ============================================================================
-- Main Render Function
-- ============================================================================

render :: World -> Picture
render world = pictures
  [ renderWorld world
  , renderUI world
  , renderDebug world
  , renderControls
  ]

-- ============================================================================
-- Controls Display
-- ============================================================================

renderControls :: Picture
renderControls = translate (-780) (-420) $ pictures
  [ color (makeColor 0.8 0.8 0.8 1) $ scale 0.08 0.08 $ text "Towers: 4=Archer 5=Ballista 6=Fire 7=Frost 8=Lightning 9=Barrage 0=Guardian"
  , translate 0 (-12) $ color (makeColor 0.8 0.8 0.8 1) $ scale 0.08 0.08 $ text "Traps: Z=Spike X=Tar C=Fire V=Explosive B=Caltrops | U=Upgrade"
  , translate 0 (-24) $ color (makeColor 0.8 0.8 0.8 1) $ scale 0.08 0.08 $ text "Abilities: Q=Firestorm W=Freeze E=Repair R=Slow | Space=Pause 1/2/3=Speed"
  ]