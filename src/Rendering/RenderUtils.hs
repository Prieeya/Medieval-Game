module Rendering.RenderUtils where

import Graphics.Gloss
import Constants

-- ============================================================================
-- Outlined Text Utility
-- ============================================================================

-- Draw text with outline/shadow for better readability
drawOutlinedText :: Color -> Color -> (Float, Float) -> String -> Picture
drawOutlinedText textColor outlineColor (x, y) str =
  let
    -- Draw outline/shadow by drawing text multiple times with offset
    outline = pictures $ map (\(dx, dy) ->
      translate (x + dx) (y + dy) $ color outlineColor $ scale 0.12 0.12 $ text str
      ) [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    -- Draw main text on top
    mainText = translate x y $ color textColor $ scale 0.12 0.12 $ text str
  in pictures [outline, mainText]

-- Draw text with drop shadow
drawTextWithShadow :: Color -> Color -> (Float, Float) -> String -> Picture
drawTextWithShadow textColor shadowColor (x, y) str =
  let
    shadow = translate (x + 2) (y - 2) $ color shadowColor $ scale 0.12 0.12 $ text str
    mainText = translate x y $ color textColor $ scale 0.12 0.12 $ text str
  in pictures [shadow, mainText]

