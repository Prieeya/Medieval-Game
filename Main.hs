{-# LANGUAGE RecordWildCards #-}

-- Main.hs - Game mechanics and AI logic

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (minimumBy, delete, sortBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Set as Set

-- Import our separated modules
import Types
import Constants
import Rendering
import Utils

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
-- COLLISION DETECTION
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
-- UTILITY FUNCTIONS (Distance is now in Utils module)
-- ============================================================================

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