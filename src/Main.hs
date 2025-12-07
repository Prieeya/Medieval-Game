module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Types
import Config
import GameLoop
import Input.InputHandler
import Rendering.RenderWorld
import Rendering.RenderUI
import Rendering.RenderDebug
import Assets (loadAllSprites)
import Rendering.SpriteAnimation (globalAssets)
import Data.IORef (writeIORef, newIORef, readIORef)
import Audio.Music
import Audio.AudioCore (playSounds, initAudio, cleanupAudio)
import qualified Data.Map.Strict as M
import System.Exit (exitSuccess)

-- ============================================================================
-- Main Entry Point
-- ============================================================================

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "Medieval Siege Defense"
  putStrLn "==================================="
  putStrLn ""
  putStrLn "Loading assets..."
  assets <- loadAllSprites
  writeIORef globalAssets (Just assets)
  putStrLn "Assets loaded!"
  putStrLn ""
  putStrLn "Initializing audio..."
  musicState <- initializeMusic
  initAudio  -- Initialize sound effects system
  musicRef <- newIORef musicState
  if musicEnabled musicState && musicInitialized musicState
    then do
      putStrLn "Music system ready, attempting to play..."
      -- Start playing background music (tries multiple formats)
      musicState' <- tryPlayMusic musicState
      writeIORef musicRef musicState'
      case currentTrack musicState' of
        Just track -> putStrLn $ "✓ Music playing: " ++ track
        Nothing -> putStrLn "⚠ Warning: No music file found or loaded"
    else putStrLn "⚠ Music system not available"
  putStrLn ""
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
  
  playIO
    FullScreen
    black
    fps
    world0
    (\world -> return $ render world)
    (\event world -> do
        -- Handle pause/unpause for music
        let wasPaused = isPaused world
        let world' = handleInput event world
        let isPaused' = isPaused world'
        musicState <- readIORef musicRef
        if wasPaused /= isPaused'
          then if isPaused'
               then do musicState' <- pauseMusic musicState
                       writeIORef musicRef musicState'
               else do musicState' <- resumeMusic musicState
                       writeIORef musicRef musicState'
          else return ()
        return world'
    )
    (\dt world -> do
        -- Update music state (check if still playing, restart if needed)
        musicState <- readIORef musicRef
        musicState' <- updateMusicState musicState dt
        
        -- Update music intensity based on game state
        let newIntensity = determineIntensity world
        musicState'' <- if musicIntensity musicState' /= newIntensity
                        then setMusicIntensity musicState' newIntensity
                        else return musicState'
        writeIORef musicRef musicState''
        
        -- Run game update logic (pure)
        let world' = updateWorld dt world
        
        -- Process and play any queued sound events
        playSounds (soundEvents world')
        
        -- Clear the sound event queue for the next frame
        let world'' = world' { soundEvents = [] }
        
        -- Check for exit request
        if shouldExit world''
          then exitSuccess
          else return world''
    )
  
  -- Cleanup music on exit
  musicState <- readIORef musicRef
  cleanupMusic musicState
  cleanupAudio

-- ============================================================================
-- Music Intensity Determination
-- ============================================================================

-- Determine music intensity based on current game state
determineIntensity :: World -> MusicIntensity
determineIntensity world
  | isGameOver world = MusicDefeat
  | isVictory world = MusicVictory
  | isBossWave world = MusicIntense
  | isWaveInProgress world = MusicNormal
  | otherwise = MusicCalm
  where
    -- Check if current wave is a boss wave (every 3rd level has a boss)
    isBossWave w = 
      let level = wsLevel (waveState w)
      in level `mod` 3 == 0 && isWaveInProgress w
    
    -- Check if enemies are currently attacking
    isWaveInProgress w = 
      case wsPhase (waveState w) of
        InWave -> not (M.null (enemies w))
        _ -> False

-- ============================================================================
-- Music Helper Functions
-- ============================================================================

-- Try to play music in order: .ogg, .mp3, .wav
tryPlayMusic :: MusicState -> IO MusicState
tryPlayMusic musicState = do
  let formats = ["assets/music/background_music.ogg", 
                 "assets/music/background_music.mp3", 
                 "assets/music/background_music.wav"]
  tryFormats musicState formats
  where
    tryFormats state [] = return state
    tryFormats state (path:paths) = do
      newState <- playMusic state path
      if currentTrack newState == Just path
        then return newState  -- Successfully loaded
        else tryFormats state paths  -- Try next format

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