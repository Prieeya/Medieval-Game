{-# LANGUAGE CPP #-}
module Audio.Music where

#ifdef USE_SDL2_MIXER
import qualified SDL.Mixer as Mix
import qualified SDL as SDL
#endif
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Control.Exception (SomeException, catch)

-- ============================================================================
-- Music System
-- ============================================================================

data MusicState = MusicState
  { musicInitialized :: Bool
  , musicEnabled :: Bool
  , currentTrack :: Maybe FilePath
  , musicVolume :: Int  -- 0-128
  }

initialMusicState :: MusicState
initialMusicState = MusicState
  { musicInitialized = False
  , musicEnabled = True
  , currentTrack = Nothing
  , musicVolume = 60  -- Default volume at 60/128 (~47%)
  }

-- ============================================================================
-- Music Initialization
-- ============================================================================

initializeMusic :: IO MusicState
#ifdef USE_SDL2_MIXER
initializeMusic = do
  -- Initialize SDL
  SDL.initialize [SDL.InitAudio]
  
  -- Initialize SDL_mixer (openAudio returns IO ())
  -- Wrap in exception handling in case initialization fails
  tryInit `catch` handleError
  where
    tryInit = do
      -- Use a larger buffer size (4096) to prevent audio stuttering and looping issues
      -- Smaller buffers (like 256) can cause the same segment to repeat instead of playing the full track
      Mix.openAudio Mix.defaultAudio 4096
      hPutStrLn stderr "Music system initialized successfully."
      return initialMusicState { musicInitialized = True, musicEnabled = True }
    handleError e = do
      hPutStrLn stderr $ "Warning: Failed to initialize music: " ++ show (e :: SomeException)
      hPutStrLn stderr "Game will continue without background music."
      return initialMusicState { musicEnabled = False }
#else
  hPutStrLn stderr "Music system not compiled (SDL2_mixer not available)."
  hPutStrLn stderr "Game will run without background music."
  return initialMusicState { musicEnabled = False }
#endif

-- ============================================================================
-- Music Playback
-- ============================================================================

-- Play background music (looping)
playMusic :: MusicState -> FilePath -> IO MusicState
playMusic musicState path
  | not (musicEnabled musicState) = return musicState
  | not (musicInitialized musicState) = return musicState
  | currentTrack musicState == Just path = return musicState  -- Already playing
#ifdef USE_SDL2_MIXER
  | otherwise = do
      tryLoad `catch` handleError
  where
    tryLoad = do
      music <- Mix.load path
      -- Stop current music if playing
      Mix.haltMusic
      -- Play new music with looping
      Mix.playMusic Mix.Forever music
      hPutStrLn stderr $ "Playing music: " ++ path
      return musicState { currentTrack = Just path }
    handleError e = do
      hPutStrLn stderr $ "Warning: Failed to load music " ++ path ++ ": " ++ show (e :: SomeException)
      return musicState
#else
  | otherwise = return musicState
#endif

-- Stop music
stopMusic :: MusicState -> IO MusicState
stopMusic musicState
  | not (musicInitialized musicState) = return musicState
#ifdef USE_SDL2_MIXER
  | otherwise = do
      Mix.haltMusic
      return musicState { currentTrack = Nothing }
#else
  | otherwise = return musicState { currentTrack = Nothing }
#endif

-- Set music volume
setMusicVolume :: MusicState -> Int -> IO MusicState
setMusicVolume musicState volume
  | not (musicInitialized musicState) = return musicState
#ifdef USE_SDL2_MIXER
  | otherwise = do
      let clampedVolume = max 0 (min 128 volume)
      -- Volume control not available in this SDL2-mixer version
      -- Music will play at system volume
      return musicState { musicVolume = clampedVolume }
#else
  | otherwise = return musicState { musicVolume = volume }
#endif

-- Pause music
pauseMusic :: MusicState -> IO MusicState
pauseMusic musicState
  | not (musicInitialized musicState) = return musicState
#ifdef USE_SDL2_MIXER
  | otherwise = do
      Mix.pauseMusic
      return musicState
#else
  | otherwise = return musicState
#endif

-- Resume music
resumeMusic :: MusicState -> IO MusicState
resumeMusic musicState
  | not (musicInitialized musicState) = return musicState
#ifdef USE_SDL2_MIXER
  | otherwise = do
      Mix.resumeMusic
      return musicState
#else
  | otherwise = return musicState
#endif

-- ============================================================================
-- Cleanup
-- ============================================================================

cleanupMusic :: MusicState -> IO ()
cleanupMusic musicState
  | musicInitialized musicState = do
#ifdef USE_SDL2_MIXER
      Mix.haltMusic
      Mix.closeAudio
      SDL.quit
#else
      return ()
#endif
  | otherwise = return ()

