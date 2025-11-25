module AI.Pathfinding where

import Types
import Constants
import qualified Data.Map.Strict as M

-- ============================================================================
-- Path Following
-- ============================================================================

getNextPathPoint :: Enemy -> World -> Maybe Vec2
getNextPathPoint enemy world =
  case enemyAIState enemy of
    MovingToFort ->
      let path = M.lookup (enemySpawnSide enemy) (paths world)
      in case path of
        Just waypoints ->
          if enemyPathIndex enemy < length waypoints
          then Just (waypoints !! enemyPathIndex enemy)
          else Nothing
        Nothing -> Nothing
    InsideFort ->
      let path = insideFortPaths world
      in if enemyPathIndex enemy < length path
         then Just (path !! enemyPathIndex enemy)
         else Nothing
    _ -> Nothing

advancePathIndex :: Enemy -> Enemy
advancePathIndex enemy =
  enemy { enemyPathIndex = enemyPathIndex enemy + 1 }

hasReachedWaypoint :: Enemy -> Vec2 -> Bool
hasReachedWaypoint enemy waypoint =
  distance (enemyPos enemy) waypoint < 20

-- ============================================================================
-- Direction Calculation
-- ============================================================================

directionTo :: Vec2 -> Vec2 -> Vec2
directionTo from to =
  let dx = fst to - fst from
      dy = snd to - snd from
      len = sqrt (dx * dx + dy * dy)
  in if len > 0.01
     then (dx / len, dy / len)
     else (0, 0)

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- ============================================================================
-- Climb Point Detection
-- ============================================================================

findNearestClimbPoint :: Enemy -> World -> Maybe (Int, Vec2)
findNearestClimbPoint enemy world =
  let walls = fortWalls (fort world)
      climbableWalls = filter (\w -> case wallClimbPoint w of
                                      Just _ -> True
                                      Nothing -> False) walls
      distances = map (\w -> case wallClimbPoint w of
                              Just cp -> (wallId w, cp, distance (enemyPos enemy) cp)
                              Nothing -> (wallId w, (0,0), 999999)) climbableWalls
      validWalls = filter (\(_, _, d) -> d < 500) distances
  in case validWalls of
      [] -> Nothing
      _ -> let (wid, cp, _) = minimum' validWalls
           in Just (wid, cp)

minimum' :: [(Int, Vec2, Float)] -> (Int, Vec2, Float)
minimum' [] = error "empty list"
minimum' [x] = x
minimum' (x:xs) =
  let (_, _, d1) = x
      (wid2, cp2, d2) = minimum' xs
  in if d1 < d2 then x else (wid2, cp2, d2)