{-# LANGUAGE RecordWildCards #-}

-- Medieval Siege Simulator
-- Save this as Main.hs in your medieval-siege directory

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (minimumBy, delete)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

-- ============================================================================
-- DATA TYPES
-- ============================================================================

type Position = (Float, Float)
type GridPos = (Int, Int)

data GameState = GameState
  { castle :: Castle
  , towers :: [Tower]
  , enemies :: [Enemy]
  , projectiles :: [Projectile]
  , resources :: Resources
  , wave :: Int
  , waveTimer :: Float
  , selectedTool :: Maybe TowerType
  , gameOver :: Bool
  , victory :: Bool
  , aiMemory :: AIMemory
  , grid :: GameGrid
  }

data Castle = Castle
  { castlePos :: Position
  , castleHealth :: Int
  , castleMaxHealth :: Int
  }

data TowerType = Arrow | Cannon | Ice deriving (Eq)

data Tower = Tower
  { towerPos :: Position
  , towerType :: TowerType
  , towerCooldown :: Float
  }

data Enemy = Enemy
  { enemyPos :: Position
  , enemyHealth :: Int
  , enemyMaxHealth :: Int
  , enemySpeed :: Float
  , enemyPath :: [GridPos]
  , enemyStunned :: Float
  , enemyId :: Int
  }

data Projectile = Projectile
  { projPos :: Position
  , projTarget :: Int
  , projDamage :: Int
  , projSpeed :: Float
  , projType :: TowerType
  }

data Resources = Resources
  { gold :: Int
  }

data AIMemory = AIMemory
  { dangerZones :: Map.Map GridPos Float
  }

data GameGrid = GameGrid
  { gridWidth :: Int
  , gridHeight :: Int
  , obstacles :: Set.Set GridPos
  }

-- ============================================================================
-- CONSTANTS
-- ============================================================================

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

cellSize :: Float
cellSize = 40

gridW, gridH :: Int
gridW = windowWidth `div` round cellSize
gridH = windowHeight `div` round cellSize

towerCost :: TowerType -> Int
towerCost Arrow = 50
towerCost Cannon = 100
towerCost Ice = 75

towerRange :: TowerType -> Float
towerRange Arrow = 150
towerRange Cannon = 120
towerRange Ice = 180

towerDamage :: TowerType -> Int
towerDamage Arrow = 15
towerDamage Cannon = 40
towerDamage Ice = 10

towerCooldownTime :: TowerType -> Float
towerCooldownTime Arrow = 0.5
towerCooldownTime Cannon = 2.0
towerCooldownTime Ice = 0.8

-- ============================================================================
-- INITIALIZATION
-- ============================================================================

initialState :: GameState
initialState = GameState
  { castle = Castle
      { castlePos = (350, 0)
      , castleHealth = 100
      , castleMaxHealth = 100
      }
  , towers = []
  , enemies = []
  , projectiles = []
  , resources = Resources { gold = 200 }
  , wave = 0
  , waveTimer = 3.0
  , selectedTool = Nothing
  , gameOver = False
  , victory = False
  , aiMemory = AIMemory { dangerZones = Map.empty }
  , grid = GameGrid
      { gridWidth = gridW
      , gridHeight = gridH
      , obstacles = Set.empty
      }
  }

-- ============================================================================
-- A* PATHFINDING
-- ============================================================================

data AStarNode = AStarNode
  { nodePos :: GridPos
  , nodeCost :: Float
  , nodeHeuristic :: Float
  , nodeParent :: Maybe GridPos
  } deriving (Eq)

nodeTotalCost :: AStarNode -> Float
nodeTotalCost n = nodeCost n + nodeHeuristic n

manhattanDistance :: GridPos -> GridPos -> Float
manhattanDistance (x1, y1) (x2, y2) = fromIntegral (abs (x1 - x2) + abs (y1 - y2))

getNeighbors :: GridPos -> [GridPos]
getNeighbors (x, y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

isValidPos :: GameGrid -> [Tower] -> GridPos -> Bool
isValidPos grid tws (x, y) =
  x >= 0 && x < gridWidth grid &&
  y >= 0 && y < gridHeight grid &&
  not (Set.member (x, y) (obstacles grid)) &&
  not (any (\t -> worldToGrid (towerPos t) == (x, y)) tws)

findPath :: GameGrid -> [Tower] -> AIMemory -> GridPos -> GridPos -> Maybe [GridPos]
findPath grid tws aiMem start goal =
  let initialNode = AStarNode start 0 (manhattanDistance start goal) Nothing
  in aStar grid tws aiMem goal [initialNode] Set.empty Map.empty

aStar :: GameGrid -> [Tower] -> AIMemory -> GridPos -> [AStarNode] -> Set.Set GridPos -> Map.Map GridPos GridPos -> Maybe [GridPos]
aStar _ _ _ _ [] _ _ = Nothing
aStar grid tws aiMem goal openList closedSet parentMap
  | null openList = Nothing
  | otherwise =
      let current = minimumBy (comparing nodeTotalCost) openList
          currentPos = nodePos current
      in if currentPos == goal
         then Just (reconstructPath parentMap goal)
         else
           let newClosedSet = Set.insert currentPos closedSet
               newOpenList = delete current openList
               neighbors = filter (\p -> isValidPos grid tws p && not (Set.member p newClosedSet)) (getNeighbors currentPos)
               dangerCost pos = Map.findWithDefault 0 pos (dangerZones aiMem)
               newNodes = map (\nPos ->
                 let gCost = nodeCost current + 1 + dangerCost nPos
                     hCost = manhattanDistance nPos goal
                 in AStarNode nPos gCost hCost (Just currentPos)) neighbors
               updatedParentMap = foldr (\n m -> Map.insert (nodePos n) currentPos m) parentMap newNodes
               mergedOpenList = foldr insertOrUpdate newOpenList newNodes
           in aStar grid tws aiMem goal mergedOpenList newClosedSet updatedParentMap
  where
    insertOrUpdate newNode nodes =
      case filter (\n -> nodePos n == nodePos newNode) nodes of
        [] -> newNode : nodes
        [existing] -> if nodeCost newNode < nodeCost existing
                      then newNode : delete existing nodes
                      else nodes
        _ -> nodes

reconstructPath :: Map.Map GridPos GridPos -> GridPos -> [GridPos]
reconstructPath parentMap goal =
  case Map.lookup goal parentMap of
    Nothing -> [goal]
    Just parent -> reconstructPath parentMap parent ++ [goal]

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
-- GAME LOGIC - UPDATE
-- ============================================================================

update :: Float -> GameState -> GameState
update dt state
  | gameOver state || victory state = state
  | otherwise =
      let state1 = updateWave dt state
          state2 = updateEnemies dt state1
          state3 = updateTowers dt state2
          state4 = updateProjectiles dt state3
          state5 = checkCollisions state4
          state6 = checkGameOver state5
      in state6

updateWave :: Float -> GameState -> GameState
updateWave dt state =
  if null (enemies state) && waveTimer state > 0
  then state { waveTimer = waveTimer state - dt }
  else if null (enemies state) && waveTimer state <= 0
       then spawnWave state
       else state

spawnWave :: GameState -> GameState
spawnWave state =
  let newWave = wave state + 1
      numEnemies = min 10 (3 + newWave * 2)
      spawnPositions = [(-380, 280 - fromIntegral i * 60) | i <- [0..numEnemies-1]]
      castleGrid = worldToGrid (castlePos (castle state))
      newEnemies = mapMaybe (\(idx, pos) ->
        let gridPos = worldToGrid pos
            path = findPath (grid state) (towers state) (aiMemory state) gridPos castleGrid
        in case path of
             Just p -> Just $ Enemy pos (20 + newWave * 5) (20 + newWave * 5) (30 + fromIntegral newWave * 2) p 0 idx
             Nothing -> Nothing) (zip [0..] spawnPositions)
      bonusGold = 50 + newWave * 10
  in state
       { wave = newWave
       , enemies = newEnemies
       , waveTimer = 5.0
       , resources = (resources state) { gold = gold (resources state) + bonusGold }
       }

updateEnemies :: Float -> GameState -> GameState
updateEnemies dt state =
  state { enemies = mapMaybe (updateEnemy dt state) (enemies state) }

updateEnemy :: Float -> GameState -> Enemy -> Maybe Enemy
updateEnemy dt _ enemy
  | enemyHealth enemy <= 0 = Nothing
  | enemyStunned enemy > 0 = Just enemy { enemyStunned = enemyStunned enemy - dt }
  | null (enemyPath enemy) = Just enemy
  | otherwise =
      let (ex, ey) = enemyPos enemy
          targetGrid = head (enemyPath enemy)
          (tx, ty) = gridToWorld targetGrid
          dx = tx - ex
          dy = ty - ey
          dist = sqrt (dx*dx + dy*dy)
          speed = enemySpeed enemy
      in if dist < speed * dt
         then Just enemy { enemyPos = (tx, ty), enemyPath = tail (enemyPath enemy) }
         else let nx = ex + (dx / dist) * speed * dt
                  ny = ey + (dy / dist) * speed * dt
              in Just enemy { enemyPos = (nx, ny) }

updateTowers :: Float -> GameState -> GameState
updateTowers dt state =
  let (newTowers, newProj) = foldl (updateTower dt (enemies state)) ([], projectiles state) (towers state)
  in state { towers = newTowers, projectiles = newProj }

updateTower :: Float -> [Enemy] -> ([Tower], [Projectile]) -> Tower -> ([Tower], [Projectile])
updateTower dt enms (tws, projs) tower =
  let newCooldown = max 0 (towerCooldown tower - dt)
      tower' = tower { towerCooldown = newCooldown }
  in if newCooldown <= 0
     then case findTarget tower enms of
            Just target ->
              let proj = Projectile (towerPos tower) (enemyId target) (towerDamage (towerType tower)) 300 (towerType tower)
                  tower'' = tower' { towerCooldown = towerCooldownTime (towerType tower) }
              in (tower'' : tws, proj : projs)
            Nothing -> (tower' : tws, projs)
     else (tower' : tws, projs)

findTarget :: Tower -> [Enemy] -> Maybe Enemy
findTarget tower enms =
  let inRange = filter (\e -> distance (towerPos tower) (enemyPos e) <= towerRange (towerType tower)) enms
  in if null inRange
     then Nothing
     else Just (head inRange)

updateProjectiles :: Float -> GameState -> GameState
updateProjectiles dt state =
  let updatedProjs = mapMaybe (updateProjectile dt (enemies state)) (projectiles state)
  in state { projectiles = updatedProjs }

updateProjectile :: Float -> [Enemy] -> Projectile -> Maybe Projectile
updateProjectile dt enms proj =
  case filter (\e -> enemyId e == projTarget proj) enms of
    [] -> Nothing
    (target:_) ->
      let (px, py) = projPos proj
          (tx, ty) = enemyPos target
          dx = tx - px
          dy = ty - py
          dist = sqrt (dx*dx + dy*dy)
          speed = projSpeed proj
      in if dist < speed * dt || dist < 10
         then Nothing
         else let nx = px + (dx / dist) * speed * dt
                  ny = py + (dy / dist) * speed * dt
              in Just proj { projPos = (nx, ny) }

checkCollisions :: GameState -> GameState
checkCollisions state =
  let projectileHits = [(projTarget p, projDamage p, projType p) | p <- projectiles state]
      hitMap = foldl (\m (eid, dmg, ptype) -> Map.insertWith (\(d1,pt1) (d2,_) -> (d1+d2,pt1)) eid (dmg,ptype) m) Map.empty projectileHits
      updatedEnemies = map (\e ->
        case Map.lookup (enemyId e) hitMap of
          Nothing -> e
          Just (dmg, ptype) ->
            let stunTime = if ptype == Ice then 1.0 else 0
            in e { enemyHealth = enemyHealth e - dmg, enemyStunned = enemyStunned e + stunTime }
        ) (enemies state)
      deadEnemies = filter (\e -> enemyHealth e <= 0) updatedEnemies
      killedPositions = map (worldToGrid . enemyPos) deadEnemies
      newDangerZones = foldl (\m pos -> Map.insertWith (+) pos 0.5 m) (dangerZones (aiMemory state)) killedPositions
      newAiMemory = (aiMemory state) { dangerZones = newDangerZones }
      goldGained = length deadEnemies * 25
      aliveEnemies = filter (\e -> enemyHealth e > 0) updatedEnemies
      (damagedCastle, filteredEnemies) = foldl (\(c, es) e ->
        if null (enemyPath e) && distance (enemyPos e) (castlePos c) < 30
        then (c { castleHealth = castleHealth c - 1 }, es)
        else (c, e:es)) (castle state, []) aliveEnemies
      validProjectiles = filter (\p -> any (\e -> enemyId e == projTarget p) filteredEnemies) (projectiles state)
  in state
       { enemies = filteredEnemies
       , projectiles = validProjectiles
       , resources = (resources state) { gold = gold (resources state) + goldGained }
       , castle = damagedCastle
       , aiMemory = newAiMemory
       }

checkGameOver :: GameState -> GameState
checkGameOver state
  | castleHealth (castle state) <= 0 = state { gameOver = True }
  | wave state >= 10 && null (enemies state) = state { victory = True }
  | otherwise = state

-- ============================================================================
-- EVENT HANDLING
-- ============================================================================

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state
  | gameOver state || victory state = initialState
  | otherwise = handleClick mousePos state
handleEvent (EventKey (Char '1') Down _ _) state = state { selectedTool = Just Arrow }
handleEvent (EventKey (Char '2') Down _ _) state = state { selectedTool = Just Cannon }
handleEvent (EventKey (Char '3') Down _ _) state = state { selectedTool = Just Ice }
handleEvent (EventKey (Char 'r') Down _ _) state
  | gameOver state || victory state = initialState
  | otherwise = state
handleEvent _ state = state

handleClick :: Position -> GameState -> GameState
handleClick pos state =
  case selectedTool state of
    Nothing -> state
    Just ttype ->
      let cost = towerCost ttype
          gridPos = worldToGrid pos
          tooClose = any (\t -> distance pos (towerPos t) < cellSize * 1.5) (towers state)
          nearCastle = distance pos (castlePos (castle state)) < cellSize * 2
      in if gold (resources state) >= cost && not tooClose && not nearCastle
         then let newTower = Tower pos ttype 0
                  newObstacles = Set.insert gridPos (obstacles (grid state))
                  newGrid = (grid state) { obstacles = newObstacles }
              in state
                   { towers = newTower : towers state
                   , resources = (resources state) { gold = gold (resources state) - cost }
                   , grid = newGrid
                   }
         else state

-- ============================================================================
-- RENDERING
-- ============================================================================

render :: GameState -> Picture
render state =
  pictures
    [ drawBackground
    , drawGrid
    , drawCastle (castle state)
    , pictures (map drawTower (towers state))
    , pictures (map drawEnemy (enemies state))
    , pictures (map drawProjectile (projectiles state))
    , drawUI state
    ]

drawBackground :: Picture
drawBackground = color (greyN 0.2) (rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight))

drawGrid :: Picture
drawGrid =
  let vLines = [Line [(x, -fromIntegral windowHeight/2), (x, fromIntegral windowHeight/2)] 
                | x <- [-fromIntegral windowWidth/2, -fromIntegral windowWidth/2 + cellSize .. fromIntegral windowWidth/2]]
      hLines = [Line [(-fromIntegral windowWidth/2, y), (fromIntegral windowWidth/2, y)] 
                | y <- [-fromIntegral windowHeight/2, -fromIntegral windowHeight/2 + cellSize .. fromIntegral windowHeight/2]]
  in color (greyN 0.3) (pictures (vLines ++ hLines))

drawCastle :: Castle -> Picture
drawCastle c =
  let (x, y) = castlePos c
      healthPercent = fromIntegral (castleHealth c) / fromIntegral (castleMaxHealth c)
  in pictures
       [ translate x y (color red (rectangleSolid 60 60))
       , translate x (y + 5) (color white (scale 0.1 0.1 (text "CASTLE")))
       , translate x (y - 40) (color green (rectangleSolid (60 * healthPercent) 5))
       ]

drawTower :: Tower -> Picture
drawTower t =
  let (x, y) = towerPos t
      col = case towerType t of
              Arrow -> blue
              Cannon -> orange
              Ice -> cyan
  in translate x y (color col (circleSolid 15))

drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
      healthPercent = fromIntegral (enemyHealth e) / fromIntegral (enemyMaxHealth e)
  in pictures
       [ translate x y (color red (circleSolid 10))
       , translate x (y + 15) (color green (rectangleSolid (20 * healthPercent) 3))
       ]

drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
  in translate x y (color yellow (circleSolid 3))

drawUI :: GameState -> Picture
drawUI state =
  pictures
    [ translate (-380) 270 (scale 0.15 0.15 (color white (text ("Gold: " ++ show (gold (resources state))))))
    , translate (-380) 250 (scale 0.15 0.15 (color white (text ("Wave: " ++ show (wave state)))))
    , translate (-380) 230 (scale 0.15 0.15 (color white (text ("HP: " ++ show (castleHealth (castle state))))))
    , translate (-380) 200 (scale 0.1 0.1 (color white (text "[1] Arrow (50g)")))
    , translate (-380) 180 (scale 0.1 0.1 (color white (text "[2] Cannon (100g)")))
    , translate (-380) 160 (scale 0.1 0.1 (color white (text "[3] Ice (75g)")))
    , if gameOver state
      then pictures
             [ color (makeColor 0 0 0 0.7) (rectangleSolid 400 200)
             , translate (-100) 20 (scale 0.3 0.3 (color red (text "DEFEAT")))
             , translate (-120) (-20) (scale 0.15 0.15 (color white (text "Click to restart")))
             ]
      else blank
    , if victory state
      then pictures
             [ color (makeColor 0 0 0 0.7) (rectangleSolid 400 200)
             , translate (-120) 20 (scale 0.3 0.3 (color green (text "VICTORY!")))
             , translate (-120) (-20) (scale 0.15 0.15 (color white (text "Click to restart")))
             ]
      else blank
    ]

-- ============================================================================
-- UTILITY FUNCTIONS
-- ============================================================================

distance :: Position -> Position -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
  in sqrt (dx*dx + dy*dy)

-- ============================================================================
-- MAIN
-- ============================================================================

main :: IO ()
main = play
  (InWindow "Medieval Siege Simulator" (windowWidth, windowHeight) (100, 100))
  black
  60
  initialState
  render
  handleEvent
  update