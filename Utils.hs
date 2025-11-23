-- Utils.hs - Shared utility functions

module Utils where

import Types
import Constants

-- ============================================================================
-- COORDINATE CONVERSION
-- ============================================================================

worldToGrid :: Position -> GridPos
worldToGrid (x, y) =
  let gx = floor ((x + fromIntegral windowWidth / 2) / cellSize)
      gy = floor ((fromIntegral windowHeight / 2 - y) / cellSize)
  in (gx, gy)

gridToWorld :: GridPos -> Position
gridToWorld (gx, gy) =
  let x = fromIntegral gx * cellSize - fromIntegral windowWidth / 2 + cellSize / 2
      y = fromIntegral windowHeight / 2 - fromIntegral gy * cellSize - cellSize / 2
  in (x, y)

-- ============================================================================
-- TOWER UTILITIES
-- ============================================================================

getTowerRange :: Tower -> Float
getTowerRange t = baseTowerRange (towerType t) * (1 + 0.15 * fromIntegral (towerLevel t - 1))

-- ============================================================================
-- DISTANCE CALCULATION
-- ============================================================================

distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in sqrt (dx*dx + dy*dy)