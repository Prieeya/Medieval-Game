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
      -- Shop Menu
      (Char 'b', Down) -> setBuildMode ShopMenu world
      
      -- Help Menu
      (Char 'm', Down) -> 
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
      -- ExplosiveBarrel moved to Shop or keeps 'b'? 'b' is now shop.
      -- Let's move ExplosiveBarrel to 'n' or access via Shop.
      (Char 'n', Down) -> setBuildMode (PlaceTrap ExplosiveBarrel) world
      
      -- Upgrade Mode
      (Char 'u', Down) -> setBuildMode UpgradeMode world
      
      -- Gate Upgrade
      (Char 'h', Down) -> upgradeGate world
      
      -- Cancel Build
      (SpecialKey KeyEsc, Down) -> setBuildMode NoBuild world
      
      -- Gate Repair (only if repair is pending)
      (Char 'g', Down) -> repairGateIfPending world
      (Char 'G', Down) -> repairGateIfPending world
      
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
      -- Quit button: (worldWidth/2 - 80, worldHeight/2 - 60), size 60x25
      -- Reset button: (worldWidth/2 - 80, worldHeight/2 - 90), size 60x25
      -- Note: Gloss coordinates have (0,0) at center, Y increases upward
      let quitX = halfW - 80
          quitY = halfH - 60
          resetX = halfW - 80
          resetY = halfH - 90
          buttonW = 60
          buttonH = 25
      in if mx >= quitX - buttonW/2 && mx <= quitX + buttonW/2 &&
            my <= quitY + buttonH/2 && my >= quitY - buttonH/2
         then requestQuit world
         else if mx >= resetX - buttonW/2 && mx <= resetX + buttonW/2 &&
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
      cost = Constants.gateUpgradeBaseCost + lvl * 100
      resources' = resources world
  in if resGold resources' >= cost
     then
       let newMaxHP = Constants.gateMaxHP + fromIntegral lvl * Constants.gateHPPerLevel
           -- Upgrade all gates
           gates' = map (\g -> g 
             { gateLevel = lvl + 1
             , gateMaxHP = newMaxHP
             , gateHP = newMaxHP -- Max heal
             , gateDestroyed = False -- Repair if destroyed
             }) gates
           newRes = ResourceSystem.spendGold cost resources'
           fort' = (fort world) { fortGates = gates' }
           -- Sound and message
           events = SoundUpgrade : soundEvents world
           message = Just ("All Gates Upgraded to Level " ++ show (lvl + 1) ++ "! (-" ++ show cost ++ "g)", 3.0)
       in world { fort = fort', resources = newRes, soundEvents = events, gameMessage = message }
     else 
       let message = Just ("Not enough gold! Need " ++ show cost ++ "g", 2.0)
           events = SoundError : soundEvents world
       in world { gameMessage = message, soundEvents = events }

-- ============================================================================
-- Building & Placing
-- ============================================================================

placeTower :: TowerType -> Vec2 -> World -> World
placeTower towerType pos world
  | not (isInsideFort pos) = world
  | not (isValidTowerPlacement pos world) = world
  | isAdvancedTower towerType && not (upgradeUnlocked $ upgradeUnlock world) = world
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
          maxHP = Constants.trapMaxHP trapType
          trap = Trap
            { trapId = nextEntityId world
            , trapType = trapType
            , trapPos = pos
            , trapTriggered = False
            , trapActiveTime = 0
            , trapAffectedEnemies = mempty
            , trapAnimState = initialAnim
            , trapHP = maxHP
            , trapMaxHP = maxHP
            , trapRevealed = False  -- Traps start hidden
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
      currentMaxHP = towerMaxHP tower
      newMaxHP = currentMaxHP * hpMult
      -- Restore HP to new max when upgraded (heal the tower)
      newHP = newMaxHP
  in tower
    { towerLevel = lvl
    , towerRange = towerRange tower * (1.0 + fromIntegral lvl * 0.1)
    , towerDamage = towerDamage tower * mult
    , towerFireRate = towerFireRate tower * 0.95
    , towerHP = newHP
    , towerMaxHP = newMaxHP
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
      -- Find all damaged gates and calculate total repair cost
      damagedGates = filter (\g -> gateHP g < gateMaxHP g) gates
      totalDamage = sum $ map (\g -> gateMaxHP g - gateHP g) damagedGates
      destroyedCount = length $ filter gateDestroyed damagedGates
      baseCost = round (totalDamage * Constants.gateRepairCostPerHP)
      destroyedBonus = destroyedCount * Constants.gateDestroyedBonus
      cost = max Constants.gateRepairMinCost (baseCost + destroyedBonus)
      hasEnoughGold = resGold (resources world) >= cost
      hasDamage = not (null damagedGates)
  in if hasDamage && hasEnoughGold
     then
       let -- Deduct gold
           resources' = ResourceSystem.spendGold cost (resources world)
           -- Repair all gates to full HP
           gates' = map (\g -> g { gateHP = gateMaxHP g, gateDestroyed = False }) gates
           fort' = (fort world) { fortGates = gates' }
           -- Sound and message
           events = SoundGateRepaired : soundEvents world
           gateCount = length damagedGates
           message = Just ("Repaired " ++ show gateCount ++ " gate(s)! (-" ++ show cost ++ "g)", 3.0)
           ws = waveState world
           ws' = ws { wsGateRepairPending = False }
       in world { fort = fort', resources = resources', waveState = ws', soundEvents = events, gameMessage = message }
     else if hasDamage && not hasEnoughGold
          then
            let gateCount = length damagedGates
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