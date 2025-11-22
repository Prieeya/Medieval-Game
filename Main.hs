{-# LANGUAGE RecordWildCards #-}

-- Medieval Siege Simulator - Complete Enhanced Edition
-- With Advanced AI, Polished UI, and Visual Effects

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (minimumBy, delete, sortBy, partition)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, isJust, fromMaybe, listToMaybe)
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
  , particles :: [Particle]
  , resources :: Resources
  , wave :: Int
  , waveTimer :: Float
  , selectedTool :: Maybe TowerType
  , hoveredCell :: Maybe GridPos
  , gameOver :: Bool
  , victory :: Bool
  , aiMemory :: AIMemory
  , grid :: GameGrid
  , gameTime :: Float
  , notifications :: [Notification]
  , tutorialStep :: Int
  }

data Castle = Castle
  { castlePos :: Position
  , castleHealth :: Int
  , castleMaxHealth :: Int
  , castleShield :: Int
  }

data TowerType = Arrow | Cannon | Ice | Lightning deriving (Eq, Show)

data Tower = Tower
  { towerPos :: Position
  , towerType :: TowerType
  , towerCooldown :: Float
  , towerKills :: Int
  , towerLevel :: Int
  }

data EnemyType
  = BasicEnemy
  | FastEnemy
  | TankEnemy
  | BossEnemy
  deriving (Eq, Ord, Show)

data EnemyState = Advancing | Attacking | Retreating | Regrouping deriving (Eq)

data Enemy = Enemy
  { enemyPos :: Position
  , enemyHealth :: Int
  , enemyMaxHealth :: Int
  , enemySpeed :: Float
  , enemyPath :: [GridPos]
  , enemyStunned :: Float
  , enemyId :: Int
  , enemyType :: EnemyType
  , enemyState :: EnemyState
  , enemyTarget :: Maybe Int
  , enemyRetreatTimer :: Float
  , enemyArmor :: Int
  }

data Projectile = Projectile
  { projPos :: Position
  , projTarget :: Int
  , projDamage :: Int
  , projSpeed :: Float
  , projType :: TowerType
  , projLifetime :: Float
  }

data ParticleType 
  = CircleParticle 
  | TextParticle 
  | SparkParticle
  | SmokeParticle
  deriving (Eq)

data Particle = Particle
  { partPos :: Position
  , partVel :: (Float, Float)
  , partColor :: Color
  , partLife :: Float
  , partSize :: Float
  , partText :: Maybe String
  , partType :: ParticleType
  }

data Resources = Resources
  { gold :: Int
  , mana :: Int
  , maxMana :: Int
  }

data AIMemory = AIMemory
  { dangerZones :: Map.Map GridPos Float
  , successfulPaths :: [([GridPos], Float)]
  , towerThreatMap :: Map.Map GridPos Float
  , lastWaveResult :: WaveResult
  , adaptationLevel :: Int
  , enemyLosses :: Map.Map EnemyType Int
  }

data WaveResult = WaveResult
  { enemiesLost :: Int
  , damageDealt :: Int
  , timeToComplete :: Float
  } deriving (Show)

data GameGrid = GameGrid
  { gridWidth :: Int
  , gridHeight :: Int
  , obstacles :: Set.Set GridPos
  , terrain :: Map.Map GridPos TerrainType
  }

data TerrainType = Normal | Rough | Fortress deriving (Eq)

data Notification = Notification
  { notifText :: String
  , notifColor :: Color
  , notifLife :: Float
  , notifPos :: Float
  }

-- ============================================================================
-- CONSTANTS
-- ============================================================================

windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 700

cellSize :: Float
cellSize = 35

gridW, gridH :: Int
gridW = windowWidth `div` round cellSize
gridH = windowHeight `div` round cellSize

towerCost :: TowerType -> Int
towerCost Arrow = 50
towerCost Cannon = 100
towerCost Ice = 75
towerCost Lightning = 150

baseTowerRange :: TowerType -> Float
baseTowerRange Arrow = 140
baseTowerRange Cannon = 110
baseTowerRange Ice = 170
baseTowerRange Lightning = 200

towerDamage :: TowerType -> Int
towerDamage Arrow = 15
towerDamage Cannon = 45
towerDamage Ice = 8
towerDamage Lightning = 30

towerCooldownTime :: TowerType -> Float
towerCooldownTime Arrow = 0.4
towerCooldownTime Cannon = 1.8
towerCooldownTime Ice = 0.6
towerCooldownTime Lightning = 2.5

enemyStats :: EnemyType -> (Int, Float, Int)
enemyStats BasicEnemy = (30, 35, 0)
enemyStats FastEnemy = (20, 60, 0)
enemyStats TankEnemy = (100, 20, 5)
enemyStats BossEnemy = (200, 25, 10)

enemyReward :: EnemyType -> Int
enemyReward BasicEnemy = 20
enemyReward FastEnemy = 30
enemyReward TankEnemy = 50
enemyReward BossEnemy = 100

-- ============================================================================
-- INITIALIZATION
-- ============================================================================

initialState :: GameState
initialState = GameState
  { castle = Castle
      { castlePos = (400, 0)
      , castleHealth = 100
      , castleMaxHealth = 100
      , castleShield = 20
      }
  , towers = []
  , enemies = []
  , projectiles = []
  , particles = []
  , resources = Resources { gold = 250, mana = 100, maxMana = 100 }
  , wave = 0
  , waveTimer = 5.0
  , selectedTool = Nothing
  , hoveredCell = Nothing
  , gameOver = False
  , victory = False
  , aiMemory = AIMemory 
      { dangerZones = Map.empty
      , successfulPaths = []
      , towerThreatMap = Map.empty
      , lastWaveResult = WaveResult 0 0 0
      , adaptationLevel = 0
      , enemyLosses = Map.empty
      }
  , grid = GameGrid
      { gridWidth = gridW
      , gridHeight = gridH
      , obstacles = Set.empty
      , terrain = Map.empty
      }
  , gameTime = 0
  , notifications = [Notification "Defend your castle!" green 3.0 0]
  , tutorialStep = 0
  }

-- ============================================================================
-- A* PATHFINDING WITH AI AWARENESS
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
getNeighbors (x, y) = 
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x-1,y-1), (x+1,y-1), (x-1,y+1)]

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
               dangerCost pos = Map.findWithDefault 0 pos (dangerZones aiMem) * 2.5
               towerThreat pos = Map.findWithDefault 0 pos (towerThreatMap aiMem) * 1.5
               terrainCost pos = case Map.lookup pos (terrain grid) of
                 Just Rough -> 1.5
                 Just Fortress -> 3.0
                 _ -> 1.0
               newNodes = map (\nPos ->
                 let gCost = nodeCost current + terrainCost nPos + dangerCost nPos + towerThreat nPos
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
      let state1 = state { gameTime = gameTime state + dt }
          state2 = updateWave dt state1
          state3 = updateEnemies dt state2
          state4 = updateTowers dt state3
          state5 = updateProjectiles dt state4
          state6 = updateParticles dt state5
          state7 = checkCollisions state6
          state8 = updateAIMemory state7
          state9 = checkGameOver state8
          state10 = updateNotifications dt state9
          state11 = regenerateMana dt state10
      in state11

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
      adaptLevel = adaptationLevel (aiMemory state)
      
      (numBasic, numFast, numTank, numBoss) = calculateEnemyComposition newWave adaptLevel (aiMemory state)
      
      totalEnemies = numBasic + numFast + numTank + numBoss
      spawnPositions = [(-480, 320 - fromIntegral i * 50) | i <- [0..totalEnemies-1]]
      
      enemyTypes = replicate numBasic BasicEnemy ++ 
                   replicate numFast FastEnemy ++ 
                   replicate numTank TankEnemy ++
                   replicate numBoss BossEnemy
      
      castleGrid = worldToGrid (castlePos (castle state))
      
      newEnemies = mapMaybe (\(idx, (pos, etype)) ->
        let gridPos = worldToGrid pos
            path = findPath (grid state) (towers state) (aiMemory state) gridPos castleGrid
            (hp, spd, armor) = enemyStats etype
            scaledHp = hp + (newWave - 1) * 5
            scaledSpeed = spd + fromIntegral (newWave - 1) * 1.5
        in case path of
             Just p -> Just $ Enemy pos scaledHp scaledHp scaledSpeed p 0 idx etype Advancing Nothing 0 armor
             Nothing -> Nothing) (zip [0..] (zip spawnPositions enemyTypes))
      
      bonusGold = 60 + newWave * 15
      newNotif = Notification ("Wave " ++ show newWave ++ " incoming!") yellow 2.5 0
      
  in state
       { wave = newWave
       , enemies = newEnemies
       , waveTimer = 8.0
       , resources = (resources state) { gold = gold (resources state) + bonusGold }
       , notifications = newNotif : notifications state
       }

calculateEnemyComposition :: Int -> Int -> AIMemory -> (Int, Int, Int, Int)
calculateEnemyComposition waveNum adaptLvl aiMem =
  let baseBasic = 3 + waveNum
      baseFast = max 0 (waveNum - 2)
      baseTank = max 0 (waveNum `div` 3)
      baseBoss = if waveNum `mod` 5 == 0 then 1 else 0
      
      basicLosses = Map.findWithDefault 0 BasicEnemy (enemyLosses aiMem)
      fastLosses = Map.findWithDefault 0 FastEnemy (enemyLosses aiMem)
      tankLosses = Map.findWithDefault 0 TankEnemy (enemyLosses aiMem)
      
      adjustedFast = if fastLosses < basicLosses then baseFast + adaptLvl else baseFast
      adjustedTank = if tankLosses < basicLosses then baseTank + (adaptLvl `div` 2) else baseTank
      
  in (min 8 baseBasic, min 5 adjustedFast, min 3 adjustedTank, baseBoss)

updateEnemies :: Float -> GameState -> GameState
updateEnemies dt state =
  let updatedEnemies = mapMaybe (updateEnemy dt state) (enemies state)
  in state { enemies = updatedEnemies }

updateEnemy :: Float -> GameState -> Enemy -> Maybe Enemy
updateEnemy dt state enemy
  | enemyHealth enemy <= 0 = Nothing
  | enemyStunned enemy > 0 = Just enemy { enemyStunned = max 0 (enemyStunned enemy - dt) }
  | otherwise =
      let newEnemy = updateEnemyState dt state enemy
      in Just $ moveEnemy dt newEnemy

updateEnemyState :: Float -> GameState -> Enemy -> Enemy
updateEnemyState dt state enemy =
  let healthPercent = fromIntegral (enemyHealth enemy) / fromIntegral (enemyMaxHealth enemy)
      nearbyTowers = filter (\t -> distance (enemyPos enemy) (towerPos t) < getTowerRange t * 1.2) (towers state)
      shouldRetreat = healthPercent < 0.3 && not (null nearbyTowers) && enemyType enemy /= TankEnemy
      
  in case enemyState enemy of
       Advancing -> 
         if shouldRetreat
         then enemy { enemyState = Retreating, enemyRetreatTimer = 3.0 }
         else enemy
       
       Retreating ->
         if enemyRetreatTimer enemy <= 0
         then enemy { enemyState = Regrouping, enemyRetreatTimer = 2.0 }
         else enemy { enemyRetreatTimer = enemyRetreatTimer enemy - dt }
       
       Regrouping ->
         if enemyRetreatTimer enemy <= 0 && healthPercent > 0.5
         then enemy { enemyState = Advancing, enemyRetreatTimer = 0 }
         else enemy { enemyRetreatTimer = enemyRetreatTimer enemy - dt }
       
       Attacking -> enemy

getTowerRange :: Tower -> Float
getTowerRange t = baseTowerRange (towerType t) * (1 + 0.15 * fromIntegral (towerLevel t - 1))

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy dt enemy
  | null (enemyPath enemy) = enemy
  | enemyState enemy == Retreating =
      let (ex, ey) = enemyPos enemy
          dx = ex + (if ex < 0 then -1 else 1) * enemySpeed enemy * dt
      in enemy { enemyPos = (dx, ey) }
  | enemyState enemy == Regrouping = enemy
  | otherwise =
      let (ex, ey) = enemyPos enemy
          targetGrid = head (enemyPath enemy)
          (tx, ty) = gridToWorld targetGrid
          dx = tx - ex
          dy = ty - ey
          dist = sqrt (dx*dx + dy*dy)
          speed = enemySpeed enemy
      in if dist < speed * dt
         then enemy { enemyPos = (tx, ty), enemyPath = tail (enemyPath enemy) }
         else let nx = ex + (dx / dist) * speed * dt
                  ny = ey + (dy / dist) * speed * dt
              in enemy { enemyPos = (nx, ny) }

updateTowers :: Float -> GameState -> GameState
updateTowers dt state =
  let (newTowers, newProj, newParts) = foldl (updateTower dt (enemies state)) ([], projectiles state, particles state) (towers state)
  in state { towers = newTowers, projectiles = newProj, particles = newParts }

updateTower :: Float -> [Enemy] -> ([Tower], [Projectile], [Particle]) -> Tower -> ([Tower], [Projectile], [Particle])
updateTower dt enms (tws, projs, parts) tower =
  let newCooldown = max 0 (towerCooldown tower - dt)
      tower' = tower { towerCooldown = newCooldown }
  in if newCooldown <= 0
     then case findTarget tower enms of
            Just target ->
              let dmg = towerDamage (towerType tower) + (towerLevel tower - 1) * 5
                  proj = Projectile (towerPos tower) (enemyId target) dmg 350 (towerType tower) 3.0
                  tower'' = tower' { towerCooldown = towerCooldownTime (towerType tower) }
                  flashPart = Particle (towerPos tower) (0, 0) yellow 0.2 8 Nothing CircleParticle
              in (tower'' : tws, proj : projs, flashPart : parts)
            Nothing -> (tower' : tws, projs, parts)
     else (tower' : tws, projs, parts)

findTarget :: Tower -> [Enemy] -> Maybe Enemy
findTarget tower enms =
  let inRange = filter (\e -> distance (towerPos tower) (enemyPos e) <= getTowerRange tower && enemyState e /= Retreating) enms
      prioritized = case towerType tower of
        Arrow -> sortBy (comparing (negate . enemyHealth)) inRange
        Cannon -> sortBy (comparing (negate . enemyMaxHealth)) inRange
        Ice -> sortBy (comparing (distance (towerPos tower) . enemyPos)) inRange
        Lightning -> sortBy (comparing (negate . enemySpeed)) inRange
  in listToMaybe prioritized

updateProjectiles :: Float -> GameState -> GameState
updateProjectiles dt state =
  let updatedProjs = mapMaybe (updateProjectile dt (enemies state)) (projectiles state)
  in state { projectiles = updatedProjs }

updateProjectile :: Float -> [Enemy] -> Projectile -> Maybe Projectile
updateProjectile dt enms proj =
  let proj' = proj { projLifetime = projLifetime proj - dt }
  in if projLifetime proj' <= 0
     then Nothing
     else case filter (\e -> enemyId e == projTarget proj) enms of
       [] -> Nothing
       (target:_) ->
         let (px, py) = projPos proj
             (tx, ty) = enemyPos target
             dx = tx - px
             dy = ty - py
             dist = sqrt (dx*dx + dy*dy)
             speed = projSpeed proj
         in if dist < speed * dt || dist < 15
            then Nothing
            else let nx = px + (dx / dist) * speed * dt
                     ny = py + (dy / dist) * speed * dt
                 in Just proj' { projPos = (nx, ny) }

updateParticles :: Float -> GameState -> GameState
updateParticles dt state =
  let updatedParts = mapMaybe (updateParticle dt) (particles state)
  in state { particles = updatedParts }

updateParticle :: Float -> Particle -> Maybe Particle
updateParticle dt part =
  let newLife = partLife part - dt
  in if newLife <= 0
     then Nothing
     else let (vx, vy) = partVel part
              (px, py) = partPos part
              newPos = (px + vx * dt, py + vy * dt)
              newVel = (vx * 0.98, vy * 0.98 - 50 * dt)
          in Just $ part { partPos = newPos, partVel = newVel, partLife = newLife }

-- ============================================================================
-- ENHANCED COLLISION DETECTION WITH DAMAGE TEXT
-- ============================================================================

createDamageText :: Position -> Int -> Particle
createDamageText pos dmg =
  Particle
    { partPos = pos
    , partVel = (0, 40)
    , partColor = if dmg > 30 then orange else yellow
    , partLife = 1.2
    , partSize = 12 + fromIntegral dmg * 0.2
    , partText = Just (show dmg)
    , partType = TextParticle
    }

checkCollisions :: GameState -> GameState
checkCollisions state =
  let projectileHits = [(projTarget p, projDamage p, projType p, projPos p) | p <- projectiles state]
      hitMap = foldl (\m (eid, dmg, ptype, pos) -> 
        Map.insertWith (\(d1,pt1,p1) (d2,_,_) -> (d1+d2,pt1,p1)) eid (dmg,ptype,pos) m) Map.empty projectileHits
      
      damageTextParticles = Map.foldlWithKey (\parts eid (dmg, _, pos) ->
        let enemy = filter (\e -> enemyId e == eid) (enemies state)
        in case enemy of
             (e:_) -> createDamageText (enemyPos e) dmg : parts
             [] -> parts
        ) [] hitMap
      
      updatedEnemies = map (\e ->
        case Map.lookup (enemyId e) hitMap of
          Nothing -> e
          Just (dmg, ptype, hitPos) ->
            let actualDmg = max 1 (dmg - enemyArmor e)
                stunTime = case ptype of
                  Ice -> 1.2
                  Lightning -> 0.5
                  _ -> 0
                newHealth = enemyHealth e - actualDmg
            in e { enemyHealth = newHealth, enemyStunned = enemyStunned e + stunTime }
        ) (enemies state)
      
      deadEnemies = filter (\e -> enemyHealth e <= 0) updatedEnemies
      killedPositions = map (worldToGrid . enemyPos) deadEnemies
      
      deathParticles = concatMap createEnhancedDeathParticles deadEnemies
      
      newDangerZones = foldl (\m pos -> Map.insertWith (+) pos 1.0 m) (dangerZones (aiMemory state)) killedPositions
      
      newLosses = foldl (\m e -> Map.insertWith (+) (enemyType e) 1 m) (enemyLosses (aiMemory state)) deadEnemies
      
      -- Update tower kill counts
      towersWithKills = map (\t ->
        let killsFromThisTower = length $ filter (\e -> 
              distance (towerPos t) (enemyPos e) <= getTowerRange t) deadEnemies
        in t { towerKills = towerKills t + killsFromThisTower }
        ) (towers state)
      
      goldGained = sum $ map (enemyReward . enemyType) deadEnemies
      aliveEnemies = filter (\e -> enemyHealth e > 0) updatedEnemies
      
      (damagedCastle, filteredEnemies, dmgParticles) = foldl (\(c, es, ps) e ->
        if null (enemyPath e) && distance (enemyPos e) (castlePos c) < 40
        then let dmg = if castleShield c > 0 then 0 else 2
                 newShield = max 0 (castleShield c - 1)
                 newHealth = castleHealth c - dmg
                 hitParts = [Particle (castlePos c) (60 * cos a, 60 * sin a) red 0.8 5 Nothing SparkParticle | a <- [0, pi/8..2*pi]]
                       ++ [Particle (castlePos c) (0, 20) yellow 1.0 15 (Just $ "-" ++ show dmg) TextParticle | dmg > 0]
             in (c { castleHealth = newHealth, castleShield = newShield }, es, hitParts ++ ps)
        else (c, e:es, ps)) (castle state, [], []) aliveEnemies
      
      validProjectiles = filter (\p -> any (\e -> enemyId e == projTarget p) filteredEnemies) (projectiles state)
      
      allParticles = damageTextParticles ++ deathParticles ++ dmgParticles ++ particles state
      
      newAiMem = (aiMemory state) { dangerZones = newDangerZones, enemyLosses = newLosses }
      
  in state
       { enemies = filteredEnemies
       , projectiles = validProjectiles
       , particles = allParticles
       , resources = (resources state) { gold = gold (resources state) + goldGained }
       , castle = damagedCastle
       , aiMemory = newAiMem
       , towers = towersWithKills
       }

createEnhancedDeathParticles :: Enemy -> [Particle]
createEnhancedDeathParticles enemy =
  let (x, y) = enemyPos enemy
      sparks = [Particle (x, y) (80 * cos a, 80 * sin a) orange 0.6 4 Nothing SparkParticle 
               | a <- [0, pi/6..2*pi]]
      smoke = [Particle (x, y) (20 * cos a, 20 * sin a) white 1.2 6 Nothing SmokeParticle 
              | a <- [pi/4, 3*pi/4, 5*pi/4, 7*pi/4]]
      circles = [Particle (x, y) (60 * cos a, 60 * sin a) red 0.5 3 Nothing CircleParticle 
                | a <- [0, pi/3..2*pi]]
  in sparks ++ smoke ++ circles

updateAIMemory :: GameState -> GameState
updateAIMemory state =
  let currentMem = aiMemory state
      newThreatMap = foldl (\m t -> 
        let gpos = worldToGrid (towerPos t)
            threat = fromIntegral (towerKills t + 1) * 0.5
        in Map.insert gpos threat m) Map.empty (towers state)
      
      newAdaptLevel = if wave state `mod` 3 == 0 then adaptationLevel currentMem + 1 else adaptationLevel currentMem
      
  in state { aiMemory = currentMem { towerThreatMap = newThreatMap, adaptationLevel = newAdaptLevel } }

checkGameOver :: GameState -> GameState
checkGameOver state
  | castleHealth (castle state) <= 0 = state { gameOver = True }
  | wave state >= 15 && null (enemies state) = state { victory = True }
  | otherwise = state

updateNotifications :: Float -> GameState -> GameState
updateNotifications dt state =
  let updatedNotifs = mapMaybe (\n -> 
        let newLife = notifLife n - dt
            newPos = notifPos n + dt * 30
        in if newLife <= 0 then Nothing else Just (n { notifLife = newLife, notifPos = newPos })) (notifications state)
  in state { notifications = updatedNotifs }

regenerateMana :: Float -> GameState -> GameState
regenerateMana dt state =
  let res = resources state
      newMana = min (maxMana res) (mana res + floor (dt * 10))
  in state { resources = res { mana = newMana } }

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
handleEvent (EventKey (Char '4') Down _ _) state = state { selectedTool = Just Lightning }
handleEvent (EventKey (Char 'u') Down _ _) state = upgradeTowerAtMouse state
handleEvent (EventKey (Char 's') Down _ _) state = sellTowerAtMouse state
handleEvent (EventKey (Char 'r') Down _ _) state
  | gameOver state || victory state = initialState
  | otherwise = state
handleEvent (EventMotion mousePos) state = state { hoveredCell = Just (worldToGrid mousePos) }
handleEvent _ state = state

handleClick :: Position -> GameState -> GameState
handleClick pos state =
  case selectedTool state of
    Nothing -> state
    Just ttype ->
      let cost = towerCost ttype
          gridPos = worldToGrid pos
          tooClose = any (\t -> distance pos (towerPos t) < cellSize * 1.8) (towers state)
          nearCastle = distance pos (castlePos (castle state)) < cellSize * 2.5
      in if gold (resources state) >= cost && not tooClose && not nearCastle
         then let newTower = Tower pos ttype 0 0 1
                  newObstacles = Set.insert gridPos (obstacles (grid state))
                  newGrid = (grid state) { obstacles = newObstacles }
                  newNotif = Notification ("Tower built!") green 1.5 0
              in state
                   { towers = newTower : towers state
                   , resources = (resources state) { gold = gold (resources state) - cost }
                   , grid = newGrid
                   , notifications = newNotif : notifications state
                   }
         else state

upgradeTowerAtMouse :: GameState -> GameState
upgradeTowerAtMouse state =
  case hoveredCell state of
    Nothing -> state
    Just cell ->
      let nearbyTowers = filter (\t -> worldToGrid (towerPos t) == cell) (towers state)
      in case nearbyTowers of
           [] -> state
           (tower:_) ->
             let upgradeCost = 50 * towerLevel tower
             in if gold (resources state) >= upgradeCost && towerLevel tower < 3
                then let upgradedTower = tower { towerLevel = towerLevel tower + 1 }
                         otherTowers = filter (\t -> towerPos t /= towerPos tower) (towers state)
                         newNotif = Notification ("Tower upgraded!") cyan 1.5 0
                     in state
                          { towers = upgradedTower : otherTowers
                          , resources = (resources state) { gold = gold (resources state) - upgradeCost }
                          , notifications = newNotif : notifications state
                          }
                else state

sellTowerAtMouse :: GameState -> GameState
sellTowerAtMouse state =
  case hoveredCell state of
    Nothing -> state
    Just cell ->
      let nearbyTowers = filter (\t -> worldToGrid (towerPos t) == cell) (towers state)
      in case nearbyTowers of
           [] -> state
           (tower:_) ->
             let sellValue = (towerCost (towerType tower) + 50 * (towerLevel tower - 1)) `div` 2
                 otherTowers = filter (\t -> towerPos t /= towerPos tower) (towers state)
                 gridPos = worldToGrid (towerPos tower)
                 newObstacles = Set.delete gridPos (obstacles (grid state))
                 newGrid = (grid state) { obstacles = newObstacles }
                 newNotif = Notification ("Tower sold!") orange 1.5 0
             in state
                  { towers = otherTowers
                  , resources = (resources state) { gold = gold (resources state) + sellValue }
                  , grid = newGrid
                  , notifications = newNotif : notifications state
                  }

-- ============================================================================
-- ENHANCED PARTICLE RENDERING
-- ============================================================================

drawFloatingTextParticle :: Particle -> Picture
drawFloatingTextParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p / 1.2)
      fadeColor = partColor p
      txt = fromMaybe "" (partText p)
      scaleFactor = partSize p * 0.01
      (r, g, b, _) = rgbaOfColor fadeColor
  in pictures
       [ translate (x + 2) (y - 2) (scale scaleFactor scaleFactor (color (makeColor 0 0 0 (alpha * 0.5)) (text txt)))
       , translate x y (scale scaleFactor scaleFactor (color (makeColor r g b alpha) (text txt)))
       ]

drawCircleParticle :: Particle -> Picture
drawCircleParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p)
      (r, g, b, _) = rgbaOfColor (partColor p)
      fadedColor = makeColor r g b alpha
  in translate x y (color fadedColor (circleSolid (partSize p)))

drawSparkParticle :: Particle -> Picture
drawSparkParticle p =
  let (x, y) = partPos p
      alpha = min 1.0 (partLife p)
      (r, g, b, _) = rgbaOfColor (partColor p)
      fadedColor = makeColor r g b alpha
      (vx, vy) = partVel p
      angle = atan2 vy vx
  in translate x y (rotate (angle * 180 / pi) (color fadedColor (rectangleSolid (partSize p * 3) (partSize p * 0.5))))

drawSmokeParticle :: Particle -> Picture
drawSmokeParticle p =
  let (x, y) = partPos p
      alpha = min 0.4 (partLife p * 0.4)
      size = partSize p * (2 - partLife p)
  in translate x y (color (makeColor 0.5 0.5 0.5 alpha) (circleSolid size))

drawParticle :: Particle -> Picture
drawParticle p =
  case partType p of
    TextParticle -> drawFloatingTextParticle p
    CircleParticle -> drawCircleParticle p
    SparkParticle -> drawSparkParticle p
    SmokeParticle -> drawSmokeParticle p

-- ============================================================================
-- POLISHED TOWER DESIGNS
-- ============================================================================

drawArrowTower :: Int -> Picture
drawArrowTower level =
  let baseColor = makeColor 0.5 0.5 0.5 1
      accentColor = makeColor 0.7 0.5 0.3 1
      size = 18 + fromIntegral (level - 1) * 2
  in pictures
       [ color baseColor (rectangleSolid size size)
       , color baseColor (translate (-size/2) (size/2) (rectangleSolid (size/4) (size/4)))
       , color baseColor (translate (size/2) (size/2) (rectangleSolid (size/4) (size/4)))
       , color baseColor (translate 0 (size/2) (rectangleSolid (size/4) (size/4)))
       , color black (rectangleSolid 3 (size * 0.6))
       , color accentColor (translate 0 (-size/3) (rectangleSolid (size * 0.8) 3))
       , color white (Polygon [(0, size/2 + 3), (-3, size/2 - 2), (3, size/2 - 2)])
       ]

drawCannonTower :: Int -> Picture
drawCannonTower level =
  let baseColor = makeColor 0.4 0.3 0.2 1
      metalColor = makeColor 0.6 0.6 0.6 1
      size = 20 + fromIntegral (level - 1) * 2
  in pictures
       [ color baseColor (rectangleSolid (size * 1.2) (size * 1.2))
       , color (dark baseColor) (rectangleSolid (size * 1.1) (size * 1.1))
       , color metalColor (rectangleSolid (size * 1.3) 6)
       , color (dark metalColor) (rectangleSolid (size * 1.3) 4)
       , translate (size * 0.65) 0 (color metalColor (circleSolid 7))
       , translate (size * 0.65) 0 (color black (circleSolid 4))
       , color metalColor (translate (-size/2) (size/2) (rectangleSolid 4 4))
       , color metalColor (translate (size/2) (size/2) (rectangleSolid 4 4))
       , color metalColor (translate (-size/2) (-size/2) (rectangleSolid 4 4))
       , color metalColor (translate (size/2) (-size/2) (rectangleSolid 4 4))
       ]

drawIceTower :: Int -> Picture
drawIceTower level =
  let baseColor = makeColor 0.6 0.8 1 0.9
      glowColor = makeColor 0.3 0.6 1 0.6
      size = 18 + fromIntegral (level - 1) * 2
  in pictures
       [ color glowColor (circleSolid (size * 1.5))
       , color baseColor (Polygon [(0, size * 1.2), (size * 0.7, 0), (0, -size * 1.2), (-size * 0.7, 0)])
       , color (makeColor 0.8 0.9 1 1) (Polygon [(0, size * 0.8), (size * 0.5, 0), (0, -size * 0.8), (-size * 0.5, 0)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(0, size * 1.2), (0, -size * 1.2)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(-size * 0.7, 0), (size * 0.7, 0)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(-size * 0.5, size * 0.8), (size * 0.5, -size * 0.8)])
       , color (makeColor 0.2 0.4 1 0.8) (Line [(size * 0.5, size * 0.8), (-size * 0.5, -size * 0.8)])
       , color white (circleSolid (size * 0.3))
       ]

drawLightningTower :: Int -> Picture
drawLightningTower level =
  let baseColor = makeColor 0.8 0.3 1 1
      sparkColor = makeColor 1 1 0.5 1
      size = 20 + fromIntegral (level - 1) * 2
  in pictures
       [ color (makeColor 0.6 0.2 0.8 0.5) (ThickCircle (size * 1.2) 3)
       , color baseColor (ThickCircle size 4)
       , color (makeColor 0.9 0.5 1 1) (circleSolid (size * 0.7))
       , color sparkColor (Polygon [(0, size * 0.7), (-3, size * 0.3), (0, size * 0.2), (3, -size * 0.7)])
       , rotate 120 (color sparkColor (Polygon [(0, size * 0.5), (-2, size * 0.2), (0, 0), (2, -size * 0.5)]))
       , rotate 240 (color sparkColor (Polygon [(0, size * 0.5), (-2, size * 0.2), (0, 0), (2, -size * 0.5)]))
       , color white (circleSolid (size * 0.2))
       ]

drawLevelBanners :: Int -> Float -> Float -> Picture
drawLevelBanners level x y =
  let bannerPositions = [(-12, 18), (0, 20), (12, 18)]
      activeBanners = take level bannerPositions
      bannerColor = makeColor 0.9 0.7 0.1 1
  in pictures [translate (x + dx) (y + dy) (pictures
                 [ color bannerColor (Polygon [(0, 0), (6, -3), (0, -8), (-6, -3)])
                 , color (dark bannerColor) (Line [(0, 0), (0, -8)])
                 ]) | (dx, dy) <- activeBanners]

drawTowerEnhanced :: GameState -> Tower -> Picture
drawTowerEnhanced state t =
  let (x, y) = towerPos t
      level = towerLevel t
      
      showRange = case hoveredCell state of
        Just cell -> worldToGrid (x, y) == cell
        Nothing -> False
      
      rangeCircle = if showRange
        then translate x y (color (makeColor 1 1 1 0.15) (ThickCircle (getTowerRange t) 2))
        else blank
      
      basePlatform = pictures
        [ color (makeColor 0.3 0.25 0.2 1) (rectangleSolid 32 32)
        , color (makeColor 0.4 0.35 0.3 1) (rectangleSolid 28 28)
        ]
      
      towerStructure = case towerType t of
        Arrow -> drawArrowTower level
        Cannon -> drawCannonTower level
        Ice -> drawIceTower level
        Lightning -> drawLightningTower level
      
      levelIndicators = drawLevelBanners level x y
      
      killBadge = if towerKills t > 0
        then translate x (y - 22) (pictures
               [ color (makeColor 0.8 0.7 0.1 0.8) (circleSolid 8)
               , scale 0.06 0.06 (color black (text (show (towerKills t))))
               ])
        else blank
      
  in pictures [rangeCircle, translate x y basePlatform, translate x y towerStructure, levelIndicators, killBadge]

-- ============================================================================
-- TOWER CONTEXT MENU
-- ============================================================================

drawTowerContextMenu :: GameState -> Picture
drawTowerContextMenu state =
  case hoveredCell state of
    Nothing -> blank
    Just cell ->
      let nearbyTowers = filter (\t -> worldToGrid (towerPos t) == cell) (towers state)
      in case nearbyTowers of
           [] -> blank
           (tower:_) ->
             let (tx, ty) = towerPos tower
                 menuX = tx + 40
                 menuY = ty
                 
                 menuBg = translate menuX menuY (color (makeColor 0.1 0.1 0.15 0.95) (rectangleSolid 140 120))
                 
                 towerName = case towerType tower of
                   Arrow -> "Arrow Tower"
                   Cannon -> "Cannon Tower"
                   Ice -> "Ice Tower"
                   Lightning -> "Lightning Tower"
                 
                 titleText = translate (menuX - 65) (menuY + 50) 
                            (scale 0.12 0.12 (color white (text towerName)))
                 
                 levelText = translate (menuX - 65) (menuY + 35) 
                            (scale 0.1 0.1 (color yellow (text ("Level: " ++ show (towerLevel tower)))))
                 
                 killsText = translate (menuX - 65) (menuY + 22) 
                            (scale 0.1 0.1 (color (makeColor 1 0.5 0.5 1) (text ("Kills: " ++ show (towerKills tower)))))
                 
                 rangeText = translate (menuX - 65) (menuY + 9) 
                            (scale 0.08 0.08 (color (greyN 0.7) (text ("Range: " ++ show (floor (getTowerRange tower) :: Int)))))
                 
                 upgradeCost = 50 * towerLevel tower
                 canUpgrade = towerLevel tower < 3 && gold (resources state) >= upgradeCost
                 
                 upgradeButton = if towerLevel tower < 3
                   then pictures
                     [ translate menuX (menuY - 10) (color (if canUpgrade 
                         then makeColor 0.2 0.6 0.2 0.9 
                         else makeColor 0.3 0.3 0.3 0.7) (rectangleSolid 120 22))
                     , translate (menuX - 55) (menuY - 12) 
                       (scale 0.1 0.1 (color (if canUpgrade then white else greyN 0.5) 
                       (text ("[U] " ++ show upgradeCost ++ "g"))))
                     ]
                   else translate (menuX - 50) (menuY - 12) 
                        (scale 0.09 0.09 (color (makeColor 1 0.8 0 1) (text "MAX LEVEL")))
                 
                 sellValue = (towerCost (towerType tower) + 50 * (towerLevel tower - 1)) `div` 2
                 sellButton = pictures
                   [ translate menuX (menuY - 40) (color (makeColor 0.6 0.2 0.2 0.9) (rectangleSolid 120 22))
                   , translate (menuX - 55) (menuY - 42) 
                     (scale 0.1 0.1 (color white (text ("[S] +" ++ show sellValue ++ "g"))))
                   ]
                 
                 connectionLine = color (makeColor 1 1 1 0.3) (Line [(tx, ty), (menuX - 70, menuY)])
                 
             in pictures [connectionLine, menuBg, titleText, levelText, killsText, rangeText, upgradeButton, sellButton]

-- ============================================================================
-- RENDERING
-- ============================================================================

render :: GameState -> Picture
render state =
  pictures
    [ drawBackground
    , drawGrid state
    , drawCastle (castle state)
    , drawTowerRanges state
    , pictures (map (drawTowerEnhanced state) (towers state))
    , pictures (map drawEnemy (enemies state))
    , pictures (map drawProjectile (projectiles state))
    , pictures (map drawParticle (particles state))
    , drawHoverPreview state
    , drawTowerContextMenu state
    , drawUI state
    , pictures (map drawNotification (notifications state))
    ]

drawBackground :: Picture
drawBackground = 
  pictures
    [ color (makeColor 0.15 0.2 0.15 1) (rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight))
    , color (makeColor 0.1 0.15 0.1 0.3) (pictures [translate 0 y (rectangleSolid (fromIntegral windowWidth) 3) | y <- [-300, -200..300]])
    ]

drawGrid :: GameState -> Picture
drawGrid state =
  let vLines = [Line [(x, -fromIntegral windowHeight/2), (x, fromIntegral windowHeight/2)] 
                | x <- [-fromIntegral windowWidth/2, -fromIntegral windowWidth/2 + cellSize .. fromIntegral windowWidth/2]]
      hLines = [Line [(-fromIntegral windowWidth/2, y), (fromIntegral windowWidth/2, y)] 
                | y <- [-fromIntegral windowHeight/2, -fromIntegral windowHeight/2 + cellSize .. fromIntegral windowHeight/2]]
      gridLines = color (makeColor 0.3 0.4 0.3 0.15) (pictures (vLines ++ hLines))
      
      dangerZoneVis = pictures [
        let (x, y) = gridToWorld pos
            intensity = min 1.0 (val / 3.0)
        in translate x y (color (makeColor 1 0 0 (intensity * 0.2)) (rectangleSolid cellSize cellSize))
        | (pos, val) <- Map.toList (dangerZones (aiMemory state))]
      
  in pictures [gridLines, dangerZoneVis]

drawCastle :: Castle -> Picture
drawCastle c =
  let (x, y) = castlePos c
      healthPercent = fromIntegral (castleHealth c) / fromIntegral (castleMaxHealth c)
      shieldAlpha = fromIntegral (castleShield c) / 20.0
  in pictures
       [ translate x y (color (dark red) (rectangleSolid 70 70))
       , translate x y (color red (rectangleSolid 60 60))
       , translate (x-25) (y+25) (color (dark red) (rectangleSolid 15 30))
       , translate (x+25) (y+25) (color (dark red) (rectangleSolid 15 30))
       , translate x (y+35) (color yellow (Polygon [(0,0), (10,5), (0,10)]))
       , if castleShield c > 0
           then translate x y (color (makeColor 0.3 0.6 1 (shieldAlpha * 0.4)) (ThickCircle 35 5))
           else blank
       , translate x (y-50) (scale 0.12 0.12 (color white (text "CASTLE")))
       , translate x (y-65) (color (dark red) (rectangleSolid 70 8))
       , translate (x - 35 + 35 * healthPercent) (y-65) (color green (rectangleSolid (70 * healthPercent) 8))
       , if castleShield c > 0
           then translate x (y-75) (color cyan (rectangleSolid (70 * fromIntegral (castleShield c) / 20) 5))
           else blank
       ]

drawTowerRanges :: GameState -> Picture
drawTowerRanges state =
  case (selectedTool state, hoveredCell state) of
    (Just ttype, Just cell) ->
      let (x, y) = gridToWorld cell
          range = baseTowerRange ttype
      in translate x y (color (makeColor 1 1 1 0.1) (ThickCircle range 2))
    _ -> blank

drawEnemy :: Enemy -> Picture
drawEnemy e =
  let (x, y) = enemyPos e
      healthPercent = fromIntegral (enemyHealth e) / fromIntegral (enemyMaxHealth e)
      (col, size) = case enemyType e of
        BasicEnemy -> (red, 12)
        FastEnemy -> (makeColor 1 0.5 0.5 1, 9)
        TankEnemy -> (makeColor 0.6 0.1 0.1 1, 18)
        BossEnemy -> (makeColor 0.8 0 0 1, 25)
      
      stateColor = case enemyState e of
        Retreating -> makeColor 0.5 0.5 1 1
        Regrouping -> yellow
        _ -> col
      
      enemyCircle = color stateColor (circleSolid size)
      
      armorBars = if enemyArmor e > 0
        then translate x (y - size - 5) (color (greyN 0.7) (rectangleSolid 20 3))
        else blank
      
      healthBg = translate x (y + size + 5) (color (dark red) (rectangleSolid 24 4))
      healthBar = translate (x - 12 + 12 * healthPercent) (y + size + 5) 
                   (color green (rectangleSolid (24 * healthPercent) 4))
      
      stunEffect = if enemyStunned e > 0
        then pictures [translate x y (color cyan (ThickCircle (size * 1.3) 2)),
                      translate x (y + size + 15) (scale 0.08 0.08 (color cyan (text "STUNNED")))]
        else blank
      
  in pictures [translate x y enemyCircle, armorBars, healthBg, healthBar, stunEffect]

drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x, y) = projPos p
      (col, size, shape) = case projType p of
        Arrow -> (yellow, 4, circleSolid 4)
        Cannon -> (orange, 6, circleSolid 6)
        Ice -> (cyan, 5, Polygon [(0,6), (4,-3), (-4,-3)])
        Lightning -> (makeColor 1 1 0.5 1, 5, ThickCircle 5 3)
      
      trail = color (makeColor 1 1 1 0.3) (Line [(x, y), (x - 10, y)])
      
  in pictures [trail, translate x y (color col shape)]

drawHoverPreview :: GameState -> Picture
drawHoverPreview state =
  case (selectedTool state, hoveredCell state) of
    (Just ttype, Just cell) ->
      let (x, y) = gridToWorld cell
          col = case ttype of
            Arrow -> makeColor 0.2 0.4 1 0.5
            Cannon -> makeColor 1 0.5 0 0.5
            Ice -> makeColor 0.4 0.8 1 0.5
            Lightning -> makeColor 0.8 0.2 1 0.5
          cost = towerCost ttype
          canAfford = gold (resources state) >= cost
          previewCol = if canAfford then col else makeColor 1 0 0 0.3
      in translate x y (color previewCol (circleSolid 15))
    _ -> blank

drawUI :: GameState -> Picture
drawUI state =
  let panelBg = translate (-420) 280 (color (makeColor 0.1 0.1 0.15 0.9) (rectangleSolid 140 120))
      
      goldText = translate (-480) 310 (scale 0.15 0.15 (color yellow (text ("Gold: " ++ show (gold (resources state))))))
      manaText = translate (-480) 290 (scale 0.15 0.15 (color cyan (text ("Mana: " ++ show (mana (resources state))))))
      waveText = translate (-480) 270 (scale 0.15 0.15 (color white (text ("Wave: " ++ show (wave state) ++ "/15"))))
      hpText = translate (-480) 250 (scale 0.15 0.15 (color green (text ("Castle HP: " ++ show (castleHealth (castle state))))))
      
      towerButtons = pictures
        [ drawTowerButton state (-480) 220 Arrow "1"
        , drawTowerButton state (-480) 195 Cannon "2"
        , drawTowerButton state (-480) 170 Ice "3"
        , drawTowerButton state (-480) 145 Lightning "4"
        ]
      
      helpText = translate (-480) 115 (scale 0.08 0.08 (color (greyN 0.7) (text "[U] Upgrade [S] Sell")))
      
      waveTimerDisplay = if null (enemies state) && waveTimer state > 0
        then translate 0 (-300) (scale 0.2 0.2 (color yellow (text ("Next wave in: " ++ show (ceiling (waveTimer state) :: Int)))))
        else blank
      
  in pictures
       [ panelBg
       , goldText
       , manaText
       , waveText
       , hpText
       , towerButtons
       , helpText
       , waveTimerDisplay
       , if gameOver state
         then pictures
                [ color (makeColor 0 0 0 0.8) (rectangleSolid 500 300)
                , translate (-150) 40 (scale 0.4 0.4 (color red (text "DEFEAT")))
                , translate (-180) (-20) (scale 0.18 0.18 (color white (text ("Reached Wave " ++ show (wave state)))))
                , translate (-150) (-60) (scale 0.15 0.15 (color white (text "Click to restart")))
                ]
         else blank
       , if victory state
         then pictures
                [ color (makeColor 0 0 0 0.8) (rectangleSolid 500 300)
                , translate (-180) 40 (scale 0.4 0.4 (color green (text "VICTORY!")))
                , translate (-200) (-20) (scale 0.18 0.18 (color white (text "Castle defended!")))
                , translate (-150) (-60) (scale 0.15 0.15 (color white (text "Click to restart")))
                ]
         else blank
       ]

drawTowerButton :: GameState -> Float -> Float -> TowerType -> String -> Picture
drawTowerButton state x y ttype key =
  let cost = towerCost ttype
      canAfford = gold (resources state) >= cost
      isSelected = selectedTool state == Just ttype
      
      bgCol = if isSelected
              then makeColor 0.3 0.5 0.8 0.8
              else if canAfford
                   then makeColor 0.2 0.3 0.4 0.6
                   else makeColor 0.3 0.1 0.1 0.6
      
      nameCol = if canAfford then white else greyN 0.5
      
      name = case ttype of
        Arrow -> "Arrow"
        Cannon -> "Cannon"
        Ice -> "Ice"
        Lightning -> "Lightning"
      
  in pictures
       [ translate (x + 60) y (color bgCol (rectangleSolid 110 20))
       , translate x y (scale 0.1 0.1 (color nameCol (text ("[" ++ key ++ "] " ++ name ++ " " ++ show cost ++ "g"))))
       ]

drawNotification :: Notification -> Picture
drawNotification notif =
  translate 0 (280 - notifPos notif) (scale 0.15 0.15 (color (notifColor notif) (text (notifText notif))))

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
  (InWindow "Medieval Siege Simulator - Enhanced Edition" (windowWidth, windowHeight) (100, 100))
  black
  60
  initialState
  render
  handleEvent
  update