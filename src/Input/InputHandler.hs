module Input.InputHandler where

import Graphics.Gloss.Interface.Pure.Game
import Types
import qualified Constants
import Constants (towerCost, trapCost, fortLeft, fortRight, fortTop, fortBottom)
import Config
import qualified Systems.ResourceSystem as ResourceSystem
import qualified Systems.AbilitySystem as AbilitySystem
import qualified Systems.WaveSystem as WaveSystem
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
handleKeyEvent key keyState mousePos world = 
  case buildMode (inputState world) of
    ConfirmationDialog _ confirm cancel ->
      case (key, keyState) of
        (SpecialKey KeyEnter, Down) -> confirm world
        (Char 'y', Down) -> confirm world
        (SpecialKey KeyEsc, Down) -> cancel world
        (Char 'n', Down) -> cancel world
        _ -> world -- Block other inputs
        
    ShopMenu ->
      case (key, keyState) of
        (SpecialKey KeyEsc, Down) -> setBuildMode NoBuild world
        (Char 'b', Down) -> setBuildMode NoBuild world -- Toggle off
        -- Let mouse clicks handle shop items
        (MouseButton LeftButton, Down) -> handleMouseClick mousePos world
        _ -> world
        
    HelpMenu ->
      case (key, keyState) of
        (SpecialKey KeyEsc, Down) -> setBuildMode NoBuild world
        (Char 'm', Down) -> setBuildMode NoBuild world
        _ -> world
        
    _ -> case (key, keyState) of
      -- Shop Menu (TAB key instead of B)
      (Char '\t', Down) -> setBuildMode ShopMenu world
      (SpecialKey KeyTab, Down) -> setBuildMode ShopMenu world
      
      -- Help Menu
      (Char 'm', Down) -> 
        if buildMode (inputState world) == HelpMenu
        then setBuildMode NoBuild world
        else setBuildMode HelpMenu world
      (Char 'M', Down) -> 
        if buildMode (inputState world) == HelpMenu
        then setBuildMode NoBuild world
        else setBuildMode HelpMenu world

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
      
      -- Build Modes - Towers (matching UI hotbar labels)
      (Char '4', Down) -> setBuildMode (PlaceTower ArrowTower) world
      (Char '5', Down) -> setBuildMode (PlaceTower BallistaTower) world
      (Char '6', Down) -> setBuildMode (PlaceTower FireTower) world
      (Char '7', Down) -> setBuildMode (PlaceTower TeslaTower) world
      (Char '8', Down) -> setBuildMode (PlaceTower BombardTower) world
      -- Additional towers not in hotbar
      (Char '9', Down) -> setBuildMode (PlaceTower CatapultTower) world
      (Char '0', Down) -> setBuildMode (PlaceTower CrossbowTower) world
      (Char '-', Down) -> setBuildMode (PlaceTower PoisonTower) world
      
      -- Build Modes - Traps (lowercase and uppercase)
      (Char 'z', Down) -> setBuildMode (PlaceTrap SpikeTrap) world
      (Char 'Z', Down) -> setBuildMode (PlaceTrap SpikeTrap) world
      (Char 'x', Down) -> setBuildMode (PlaceTrap FreezeTrap) world
      (Char 'X', Down) -> setBuildMode (PlaceTrap FreezeTrap) world
      (Char 'c', Down) -> setBuildMode (PlaceTrap FirePitTrap) world
      (Char 'C', Down) -> setBuildMode (PlaceTrap FirePitTrap) world
      (Char 'v', Down) -> setBuildMode (PlaceTrap MagicSnareTrap) world
      (Char 'V', Down) -> setBuildMode (PlaceTrap MagicSnareTrap) world
      (Char 'b', Down) -> setBuildMode (PlaceTrap ExplosiveBarrel) world
      (Char 'B', Down) -> setBuildMode (PlaceTrap ExplosiveBarrel) world
      
      -- Upgrade Mode
      (Char 'u', Down) -> setBuildMode UpgradeMode world
      (Char 'U', Down) -> setBuildMode UpgradeMode world
      
      -- Gate Upgrade
      (Char 'h', Down) -> upgradeGate world
      (Char 'H', Down) -> upgradeGate world
      
      -- Cancel Build
      (SpecialKey KeyEsc, Down) -> setBuildMode NoBuild world
      
      -- Gate Repair (only if repair is pending)
      (Char 'g', Down) -> repairGateIfPending world
      (Char 'G', Down) -> repairGateIfPending world
      
      -- Abilities
      (Char 'q', Down) -> AbilitySystem.activateAbility Firestorm world
      (Char 'Q', Down) -> AbilitySystem.activateAbility Firestorm world
      (Char 'w', Down) -> AbilitySystem.activateAbility FreezeField world
      (Char 'W', Down) -> AbilitySystem.activateAbility FreezeField world
      (Char 'e', Down) -> AbilitySystem.activateAbility RepairWalls world
      (Char 'E', Down) -> AbilitySystem.activateAbility RepairWalls world
      (Char 'r', Down) -> AbilitySystem.activateAbility TimeSlow world
      (Char 'R', Down) -> AbilitySystem.activateAbility TimeSlow world
      
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
      (mx, my) = mousePos
      halfW = Constants.worldWidth / 2
      halfH = Constants.worldHeight / 2
  in case mode of
    ConfirmationDialog _ confirm cancel ->
      -- Simple hit detection for Confirm/Cancel buttons
      -- Assuming dialog centered
      if my >= -20 && my <= 20
      then if mx >= -100 && mx <= -20 then confirm world
           else if mx >= 20 && mx <= 100 then cancel world
           else world
      else world
      
    ShopMenu ->
      -- Handle shop clicks
      -- Close button (approx 270, 220)
      if mx > 200 && my > 150 then setBuildMode NoBuild world 
      -- Unlock Button (approx 0, -150)
      else if not (upgradeUnlocked $ upgradeUnlock world) && 
              mx >= -125 && mx <= 125 && my >= -175 && my <= -125
           then tryUnlockUpgrades world
      else world
      
    PlaceTower towerType -> placeTower towerType worldPos world
    PlaceTrap trapType -> placeTrap trapType worldPos world
    UpgradeMode -> upgradeTowerAt worldPos world
    NoBuild -> 
      -- Check for UI button clicks (buttons are at top-right)
      -- Updated positions: 100px from right edge, buttons are 80x30
      -- Note: Gloss coordinates have (0,0) at center, Y increases upward
      let buttonX = halfW - 100  -- 100px from right edge
          quitY = halfH - 40     -- 40px from top
          resetY = halfH - 75    -- 75px from top
          buttonW = 80
          buttonH = 30
      in if mx >= buttonX - buttonW/2 && mx <= buttonX + buttonW/2 &&
            my <= quitY + buttonH/2 && my >= quitY - buttonH/2
         then requestQuit world
         else if mx >= buttonX - buttonW/2 && mx <= buttonX + buttonW/2 &&
                 my <= resetY + buttonH/2 && my >= resetY - buttonH/2
              then requestReset world
              else world

-- ============================================================================
-- Game Actions
-- ============================================================================

tryUnlockUpgrades :: World -> World
tryUnlockUpgrades world =
  let unlock = upgradeUnlock world
      cost = Constants.gateUpgradeBaseCost -- Actually use upgradeCost from Types/Config
      realCost = upgradeCost unlock
      res = resources world
  in if resGold res >= realCost
     then world { resources = ResourceSystem.spendGold realCost res
                , upgradeUnlock = unlock { upgradeUnlocked = True }
                , soundEvents = SoundUpgrade : soundEvents world
                }
     else world -- Could play error sound here

requestQuit :: World -> World
requestQuit world = 
  setBuildMode (ConfirmationDialog "Quit Game?" (\w -> w { shouldExit = True }) (\w -> setBuildMode NoBuild w)) world

requestReset :: World -> World
requestReset world =
  setBuildMode (ConfirmationDialog "Reset Game?" (\_ -> initialWorld) (\w -> setBuildMode NoBuild w)) world

upgradeGate :: World -> World
upgradeGate world =
  let gates = fortGates (fort world)
      -- Use the first gate's level as the overall gate level
      lvl = if null gates then 1 else gateLevel (head gates)
      cost = Constants.gateUpgradeCost lvl
      resources' = resources world
  in if lvl >= Constants.maxGateLevel
     then world { gameMessage = Just ("Gates already max level!", 2.0) }
     else if resGold resources' >= cost
     then
       let newLevel = lvl + 1
           newMaxHP = Constants.gateMaxHP + fromIntegral lvl * Constants.gateHPPerLevel
           -- Upgrade all gates
           gates' = map (\g -> g 
             { gateLevel = newLevel
             , gateMaxHP = newMaxHP
             , gateHP = newMaxHP -- Max heal
             , gateDestroyed = False -- Repair if destroyed
             }) gates
           newRes = ResourceSystem.spendGold cost resources'
           fort' = (fort world) { fortGates = gates' }
           -- Sound and message
           events = SoundUpgrade : soundEvents world
           bonusDesc = Constants.gateUpgradeBonus newLevel
           msg = "Gates Upgraded to Lv" ++ show newLevel ++ "! " ++ bonusDesc ++ " (-" ++ show cost ++ "g)"
       in world { fort = fort', resources = newRes, soundEvents = events, gameMessage = Just (msg, 3.0) }
     else 
       let events = SoundError : soundEvents world
       in world { gameMessage = Just ("Need " ++ show cost ++ "g to upgrade gates", 2.0), soundEvents = events }

-- ============================================================================
-- Building & Placing
-- ============================================================================

placeTower :: TowerType -> Vec2 -> World -> World
placeTower towerType pos world
  | not (isInsideFort pos) = world
  | not (isValidTowerPlacement pos world) = world
  -- Removed lock check - all towers are now available from the start
  | resGold (resources world) < towerCost towerType = world
  | otherwise =
      let tower = createTower (nextEntityId world) towerType pos (timeElapsed world)
          towers' = M.insert (nextEntityId world) tower (towers world)
          resources' = ResourceSystem.spendGold (towerCost towerType) (resources world)
          events = SoundTowerBuild : soundEvents world
          world' = world
            { towers = towers'
            , resources = resources'
            , nextEntityId = nextEntityId world + 1
            , soundEvents = events
            }
      in world'

isAdvancedTower :: TowerType -> Bool
isAdvancedTower TeslaTower = True
isAdvancedTower BallistaTower = True
isAdvancedTower BombardTower = True
isAdvancedTower _ = False

placeTrap :: TrapType -> Vec2 -> World -> World
placeTrap trapType pos world
  | not (isValidTrapPlacement pos world) = world
  | isAdvancedTrap trapType && not (upgradeUnlocked $ upgradeUnlock world) = world
  | resGold (resources world) < trapCost trapType = world
  | otherwise =
      let initialAnim = AnimationState { animType = AnimIdle, animFrame = 0, animTime = 0 }
          lvl = 1  -- New traps start at level 1
          maxHP = Constants.trapMaxHPForLevel trapType lvl
          dmg = Constants.trapDamageForLevel trapType lvl
          trap = Trap
            { trapId = nextEntityId world
            , trapType = trapType
            , trapPos = pos
            , trapLevel = lvl
            , trapTriggered = False
            , trapActiveTime = 0
            , trapAffectedEnemies = mempty
            , trapAnimState = initialAnim
            , trapHP = maxHP
            , trapMaxHP = maxHP
            , trapRevealed = False  -- Traps start hidden
            , trapDamage = dmg
            }
          traps' = M.insert (nextEntityId world) trap (traps world)
          resources' = ResourceSystem.spendGold (trapCost trapType) (resources world)
          world' = world
            { traps = traps'
            , resources = resources'
            , nextEntityId = nextEntityId world + 1
            }
      in world'

isAdvancedTrap :: TrapType -> Bool
isAdvancedTrap ExplosiveBarrel = True
isAdvancedTrap MagicSnareTrap = True
isAdvancedTrap _ = False

upgradeTowerAt :: Vec2 -> World -> World
upgradeTowerAt pos world =
  -- First check for tower, then check for trap
  case findTowerAt pos world of
    Just tower -> upgradeTowerIfPossible tower world
    Nothing -> 
      case findTrapAt pos world of
        Just trap -> upgradeTrapIfPossible trap world
        Nothing -> world

upgradeTowerIfPossible :: Tower -> World -> World
upgradeTowerIfPossible tower world =
  let currentLevel = towerLevel tower
      cost = Constants.towerUpgradeCost (towerType tower) currentLevel
  in if currentLevel >= Constants.maxTowerLevel
     then world { gameMessage = Just ("Tower already max level!", 2.0) }
     else if resGold (resources world) < cost
     then world { gameMessage = Just ("Need " ++ show cost ++ "g to upgrade", 2.0), soundEvents = SoundError : soundEvents world }
     else
       let tower' = upgradeTower tower
           towers' = M.insert (towerId tower) tower' (towers world)
           resources' = ResourceSystem.spendGold cost (resources world)
           msg = "Tower upgraded to Lv" ++ show (currentLevel + 1) ++ "! (-" ++ show cost ++ "g)"
       in world { towers = towers', resources = resources', gameMessage = Just (msg, 3.0), soundEvents = SoundUpgrade : soundEvents world }

upgradeTrapIfPossible :: Trap -> World -> World
upgradeTrapIfPossible trap world =
  let currentLevel = trapLevel trap
      cost = Constants.trapUpgradeCost (trapType trap) currentLevel
  in if currentLevel >= Constants.maxTrapLevel
     then world { gameMessage = Just ("Trap already max level!", 2.0) }
     else if resGold (resources world) < cost
     then world { gameMessage = Just ("Need " ++ show cost ++ "g to upgrade", 2.0), soundEvents = SoundError : soundEvents world }
     else
       let trap' = upgradeTrap trap
           traps' = M.insert (trapId trap) trap' (traps world)
           resources' = ResourceSystem.spendGold cost (resources world)
           msg = "Trap upgraded to Lv" ++ show (currentLevel + 1) ++ "! (-" ++ show cost ++ "g)"
       in world { traps = traps', resources = resources', gameMessage = Just (msg, 3.0), soundEvents = SoundUpgrade : soundEvents world }

upgradeTower :: Tower -> Tower
upgradeTower tower =
  let newLevel = towerLevel tower + 1
      tt = towerType tower
      (newRange, newDmg, newFR) = Constants.towerStatsForLevel tt newLevel
      newMaxHP = Constants.towerMaxHPForLevel tt newLevel
  in tower
    { towerLevel = newLevel
    , towerRange = newRange
    , towerDamage = newDmg
    , towerFireRate = newFR
    , towerHP = newMaxHP  -- Full heal on upgrade
    , towerMaxHP = newMaxHP
    }

upgradeTrap :: Trap -> Trap
upgradeTrap trap =
  let newLevel = trapLevel trap + 1
      tt = trapType trap
      newMaxHP = Constants.trapMaxHPForLevel tt newLevel
      newDmg = Constants.trapDamageForLevel tt newLevel
  in trap
    { trapLevel = newLevel
    , trapHP = newMaxHP  -- Full heal on upgrade
    , trapMaxHP = newMaxHP
    , trapDamage = newDmg
    }

findTowerAt :: Vec2 -> World -> Maybe Tower
findTowerAt pos world =
  let nearbyTowers = M.filter (\t -> distance (towerPos t) pos < 30) (towers world)
  in case M.elems nearbyTowers of
    [] -> Nothing
    (t:_) -> Just t

findTrapAt :: Vec2 -> World -> Maybe Trap
findTrapAt pos world =
  let nearbyTraps = M.filter (\t -> distance (trapPos t) pos < 30) (traps world)
  in case M.elems nearbyTraps of
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
      gates = fortGates (fort world)
      tooCloseToGate = any (\g -> distance pos (gatePos g) < 60) gates
      tooCloseToCastle = distance pos (castlePos $ castle world) < 100
  in not (tooCloseToTower || tooCloseToGate || tooCloseToCastle)

isValidTrapPlacement :: Vec2 -> World -> Bool
isValidTrapPlacement pos world =
  let tooCloseToTrap = any (\t -> distance (trapPos t) pos < 30) (M.elems $ traps world)
  in not tooCloseToTrap

-- ============================================================================
-- Gate Repair (repairs all damaged gates)
-- ============================================================================

repairGateIfPending :: World -> World
repairGateIfPending world =
  let gates = fortGates (fort world)
      -- Find only destroyed gates (not just damaged ones)
      destroyedGates = filter gateDestroyed gates
      -- Flat cost: 150 gold per destroyed gate only
      cost = length destroyedGates * 150
      hasEnoughGold = resGold (resources world) >= cost
      hasDamage = not (null destroyedGates)
  in if hasDamage && hasEnoughGold
     then
       let -- Deduct gold
           resources' = ResourceSystem.spendGold cost (resources world)
           -- Repair only destroyed gates to full HP
           gates' = map (\g -> if gateDestroyed g 
                              then g { gateHP = gateMaxHP g, gateDestroyed = False }
                              else g) gates
           fort' = (fort world) { fortGates = gates' }
           -- Sound and message
           events = SoundGateRepaired : soundEvents world
           gateCount = length destroyedGates
           message = Just ("Repaired " ++ show gateCount ++ " gate(s)! (-" ++ show cost ++ "g)", 3.0)
           ws = waveState world
           ws' = ws { wsGateRepairPending = False }
       in world { fort = fort', resources = resources', waveState = ws', soundEvents = events, gameMessage = message }
     else if hasDamage && not hasEnoughGold
          then
            let gateCount = length destroyedGates
                message = Just ("Need " ++ show cost ++ "g to repair " ++ show gateCount ++ " gate(s)", 2.0)
                events = SoundError : soundEvents world
            in world { gameMessage = message, soundEvents = events }
          else world  -- Do nothing if conditions not met

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