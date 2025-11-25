module Rendering.RenderDebug where

import Graphics.Gloss
import Types
import qualified Data.Map.Strict as M

-- ============================================================================
-- Debug Rendering
-- ============================================================================

renderDebug :: World -> Picture
renderDebug world
  | not (showDebug world) = blank
  | otherwise = translate (-400) 300 $ pictures
    [ color white $ scale 0.1 0.1 $ text $ "Enemies: " ++ show (M.size $ enemies world)
    , translate 0 (-15) $ color white $ scale 0.1 0.1 $ text $ "Towers: " ++ show (M.size $ towers world)
    , translate 0 (-30) $ color white $ scale 0.1 0.1 $ text $ "Projectiles: " ++ show (M.size $ projectiles world)
    , translate 0 (-45) $ color white $ scale 0.1 0.1 $ text $ "FPS Target: 60"
    ]