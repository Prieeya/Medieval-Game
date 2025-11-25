module Input.InputHandler where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Constants
import Config
import qualified Systems.ResourceSystem as ResourceSystem
import qualified Systems.AbilitySystem as AbilitySystem
import qualified Data.Map.Strict as M

-- ============================================================================
-- Event Handler
-- ============================================================================

handleInput :: Event -> World -> World
handleInput event world = case event of
  EventKey key keyState _ mousePos ->
    handleKeyEvent key keyState mousePos world
  EventMotion mousePos ->
    handleMouseMove mousePos world
  _ -> world

-- ============================================================================
-- Key Events
-- ============================================================================

handleKeyEvent :: Key -> KeyState -> (Float, Float) -> World -> World
handleKeyEvent key keyState mousePos world = case (key, keyState) of
  -- Pause
  (SpecialKey KeySpace, Down) ->
    world { isPaused = not (isPaused world) }
  
  -- Game Speed
  (Char '1', Down) -> world { gameSpeed = Speed1x }
  (Char '2', Down) -> world { gameSpeed = Speed2x }
  (Char '3', Down) -> world { gameSpeed = Speed4x }
  
  -- Debug Toggle
  (SpecialKey KeyF1, Down) ->
    world { showDebug = not (showDebug world) }
  
  -- Build Modes - Towers
  (Char '4', Down) -> setBuildMode (PlaceTower ArrowTower) world
  (Char '5', Down) -> setBuildMode (PlaceTower CatapultTower) world
  (Char '6', Down) -> setBuildMode (PlaceTower CrossbowTower) world
  (Char '7', Down) -> setBuildMode (PlaceTower FireTower) world
  (Char '8', Down) -> setBuildMode (PlaceTower TeslaTower) world
  (Char '9', Down) -> setBuildMode (PlaceTower BallistaTower) world
  (Char '0', Down) -> setBuildMode (PlaceTower PoisonTower) world
  (Char '-', Down) -> setBuildMode (PlaceTower BombardTower) world
  
  -- Build Modes - Traps
  (Char 'z', Down) -> setBuildMode (PlaceTrap SpikeTrap) world
  (Char 'x', Down) -> setBuildMode (PlaceTrap FreezeTrap) world
  (Char 'c', Down) -> setBuildMode (PlaceTrap FirePitTrap) world
  (Char 'v', Down) -> setBuildMode (PlaceTrap MagicSnareTrap) world
  (Char 'b', Down) -> setBuildMode (PlaceTrap ExplosiveBarrel) world
  
  -- Upgrade Mode
  (Char 'u', Down) -> setBuildMode UpgradeMode world
  
  -- Cancel Build
  (SpecialKey KeyEsc, Down) -> setBuildMode NoBuild world
  
  -- Abilities
  (Char 'q', Down) -> AbilitySystem.activateAbility Firestorm world
  (Char 'w', Down) -> AbilitySystem.activateAbility FreezeField world
  (Char 'e', Down) -> AbilitySystem.activateAbility RepairWalls world
  (Char 'r', Down) -> AbilitySystem.activateAbility TimeSlow world
  
  -- Mouse Click
  (MouseButton LeftButton, Down) ->
    handleMouseClick mousePos world
  
  _ -> world

-- ============================================================================
-- Mouse Handling
-- ============================================================================

handleMouseMove :: (Float, Float) -> World -> World
handleMouseMove mousePos world =
  let inputState' = (inputState world)
        { mousePos = mousePos
        , mouseWorldPos = mousePos
        , hoveredTile = Just (snapToGrid mousePos)
        }
  in world { inputState = inputState' }

handleMouseClick :: (Float, Float) -> World -> World
handleMouseClick mousePos world =
  let worldPos = mousePos
      mode = buildMode (inputState world)
  in case mode of
    PlaceTower towerType -> placeTower towerType worldPos world
    PlaceTrap trapType -> placeTrap trapType worldPos world
    UpgradeMode -> upgradeTowerAt worldPos world
    NoBuild -> world

-- ============================================================================
-- Building & Placing
-- ============================================================================

placeTower :: TowerType -> Vec2 -> World -> World
placeTower towerType pos world
  | not (isInsideFort pos) = world
  | not (isValidTowerPlacement pos world) = world
  | resGold (resources world) < towerCost towerType = world
  | otherwise =
      let tower = createTower (nextEntityId world) towerType pos (timeElapsed world)
          towers' = M.insert (nextEntityId world) tower (towers world)
          resources' = ResourceSystem.spendGold (towerCost towerType) (resources world)
          world' = world
            { towers = towers'
            , resources = resources'
            , nextEntityId = nextEntityId world + 1
            }
      in world'

placeTrap :: TrapType -> Vec2 -> World -> World
placeTrap trapType pos world
  | not (isValidTrapPlacement pos world) = world
  | resGold (resources world) < trapCost trapType = world
  | otherwise =
      let trap = Trap
            { trapId = nextEntityId world
            , trapType = trapType
            , trapPos = pos
            , trapTriggered = False
            , trapActiveTime = 0
            , trapAffectedEnemies = mempty
            }
          traps' = M.insert (nextEntityId world) trap (traps world)
          resources' = ResourceSystem.spendGold (trapCost trapType) (resources world)
          world' = world
            { traps = traps'
            , resources = resources'
            , nextEntityId = nextEntityId world + 1
            }
      in world'

upgradeTowerAt :: Vec2 -> World -> World
upgradeTowerAt pos world =
  let upUnlock = upgradeUnlock world
      level = wsLevel (waveState world)
  in case findTowerAt pos world of
    Nothing -> world
    Just tower ->
      if level < upgradeLevel upUnlock && not (upgradeUnlocked upUnlock)
      then world  -- Upgrades not unlocked yet
      else if upgradeUnlocked upUnlock && resGold (resources world) >= upgradeCost upUnlock
           then
             let tower' = upgradeTower tower
                 towers' = M.insert (towerId tower) tower' (towers world)
                 resources' = ResourceSystem.spendGold (upgradeCost upUnlock) (resources world)
             in world { towers = towers', resources = resources' }
           else
             -- Unlock upgrades if conditions met
             let upUnlock' = upUnlock { upgradeUnlocked = True }
                 resources' = ResourceSystem.spendGold (upgradeCost upUnlock) (resources world)
             in if level >= upgradeLevel upUnlock && resGold (resources world) >= upgradeCost upUnlock && not (upgradeUnlocked upUnlock)
                then 
                  let tower' = upgradeTower tower
                      towers' = M.insert (towerId tower) tower' (towers world)
                  in world { towers = towers', resources = resources', upgradeUnlock = upUnlock' }
                else world

upgradeTower :: Tower -> Tower
upgradeTower tower =
  let lvl = towerLevel tower + 1
      mult = 1.0 + fromIntegral lvl * 0.3
      hpMult = 1.0 + fromIntegral lvl * 0.2  -- HP increases by 20% per level
      currentMaxHP = Types.towerMaxHP tower
      newMaxHP = currentMaxHP * hpMult
      -- Restore HP to new max when upgraded (heal the tower)
      newHP = newMaxHP
  in tower
    { towerLevel = lvl
    , towerRange = towerRange tower * (1.0 + fromIntegral lvl * 0.1)
    , towerDamage = towerDamage tower * mult
    , towerFireRate = towerFireRate tower * 0.95
    , towerHP = newHP
    , Types.towerMaxHP = newMaxHP
    }

findTowerAt :: Vec2 -> World -> Maybe Tower
findTowerAt pos world =
  let nearbyTowers = M.filter (\t -> distance (towerPos t) pos < 30) (towers world)
  in case M.elems nearbyTowers of
    [] -> Nothing
    (t:_) -> Just t

-- ============================================================================
-- Validation Helpers
-- ============================================================================

isInsideFort :: Vec2 -> Bool
isInsideFort (x, y) =
  x >= fortLeft && x <= fortRight &&
  y >= fortBottom && y <= fortTop

isValidTowerPlacement :: Vec2 -> World -> Bool
isValidTowerPlacement pos world =
  let tooCloseToTower = any (\t -> distance (towerPos t) pos < 50) (M.elems $ towers world)
      tooCloseToGate = distance pos (gatePos $ fortGate $ fort world) < 60
      tooCloseToCastle = distance pos (castlePos $ castle world) < 100
  in not (tooCloseToTower || tooCloseToGate || tooCloseToCastle)

isValidTrapPlacement :: Vec2 -> World -> Bool
isValidTrapPlacement pos world =
  let tooCloseToTrap = any (\t -> distance (trapPos t) pos < 30) (M.elems $ traps world)
  in not tooCloseToTrap

-- ============================================================================
-- Utilities
-- ============================================================================

distance :: Vec2 -> Vec2 -> Float
distance (x1, y1) (x2, y2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2)

snapToGrid :: Vec2 -> Vec2
snapToGrid (x, y) =
  let gridSize = 30
      x' = fromIntegral (round (x / gridSize)) * gridSize
      y' = fromIntegral (round (y / gridSize)) * gridSize
  in (x', y')

setBuildMode :: BuildMode -> World -> World
setBuildMode mode world =
  let inputState' = (inputState world) { buildMode = mode }
  in world { inputState = inputState' }