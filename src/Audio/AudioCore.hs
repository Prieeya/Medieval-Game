{-# LANGUAGE CPP #-}
module Audio.AudioCore where

import Types (SoundEvent)
import Audio.SoundEvents (soundEventToFile)
import qualified Data.Map.Strict as M
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (catch, SomeException)
import System.IO (hPutStrLn, stderr)

#ifdef USE_SDL2_MIXER
import qualified SDL.Mixer as Mix
#endif

-- ============================================================================
-- Audio System Core
-- ============================================================================

-- Global cache for loaded sound chunks
-- This is safe because it's only accessed from the main thread in IO
{-# NOINLINE soundCache #-}
soundCache :: IORef (M.Map FilePath Mix.Chunk)
soundCache = unsafePerformIO $ newIORef M.empty

-- Initialize Audio System (called once at startup)
initAudio :: IO ()
initAudio = do
#ifdef USE_SDL2_MIXER
  -- Mix.openAudio is called in Music.initializeMusic.
  -- Sound effects will use available channels automatically.
  -- No need to allocate channels explicitly - SDL_mixer handles this.
#endif
  return ()

-- Play a list of sound events
playSounds :: [SoundEvent] -> IO ()
playSounds events = mapM_ playSound events

-- Play a single sound event
playSound :: SoundEvent -> IO ()
playSound event = do
#ifdef USE_SDL2_MIXER
  let path = soundEventToFile event
  cache <- readIORef soundCache
  chunk <- case M.lookup path cache of
    Just c -> return (Just c)
    Nothing -> do
      -- Try to load the file
      result <- tryLoadChunk path
      case result of
        Just c -> do
          -- Cache the loaded chunk
          modifyIORef soundCache (M.insert path c)
          return (Just c)
        Nothing -> return Nothing
  
  case chunk of
    Just c -> do
      -- Play on first available channel, don't loop
      -- Use play with exception handling to avoid "no free channels" error
      -- Catch any exceptions (including "no free channels")
      catch (do
        _ <- Mix.play c
        return ())
        handlePlayError
      where
        handlePlayError :: SomeException -> IO ()
        handlePlayError _ = do
          -- If no channels available or any other error, silently fail
          -- Too many sounds playing simultaneously - this is expected
          return ()
    Nothing -> return ()
#else
  return ()
#endif

#ifdef USE_SDL2_MIXER
tryLoadChunk :: FilePath -> IO (Maybe Mix.Chunk)
tryLoadChunk path = do
  catch (do
    chunk <- Mix.load path
    -- Volume is controlled at playback time, not per-chunk
    -- SDL_mixer will use default volume settings
    return (Just chunk))
    handleError
  where
    handleError :: SomeException -> IO (Maybe Mix.Chunk)
    handleError e = do
      -- hPutStrLn stderr $ "Warning: Could not load sound " ++ path ++ ": " ++ show e
      return Nothing
#endif

-- Cleanup audio resources
cleanupAudio :: IO ()
cleanupAudio = do
#ifdef USE_SDL2_MIXER
  cache <- readIORef soundCache
  mapM_ Mix.free (M.elems cache)
  writeIORef soundCache M.empty
#endif
  return ()