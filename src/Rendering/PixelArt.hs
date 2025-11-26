module Rendering.PixelArt where

import Graphics.Gloss
import Types (AnimationType(..), TowerType(..), UnitType(..), TrapType(..), ProjectileType(..), EnemyAIState(..))

-- ============================================================================
-- Pixel Art Color Palette
-- ============================================================================

-- Color definitions based on palette names
pixelColor :: String -> Color
pixelColor "brown" = makeColor 0.4 0.25 0.15 1
pixelColor "dark brown" = makeColor 0.25 0.15 0.1 1
pixelColor "tan" = makeColor 0.6 0.5 0.4 1
pixelColor "black" = makeColor 0.0 0.0 0.0 1
pixelColor "gray" = makeColor 0.5 0.5 0.5 1
pixelColor "dark gray" = makeColor 0.3 0.3 0.3 1
pixelColor "silver" = makeColor 0.75 0.75 0.75 1
pixelColor "red" = makeColor 0.8 0.2 0.2 1
pixelColor "orange" = makeColor 1.0 0.5 0.0 1
pixelColor "yellow" = makeColor 1.0 1.0 0.0 1
pixelColor "blue" = makeColor 0.2 0.4 0.8 1
pixelColor "light blue" = makeColor 0.4 0.6 1.0 1
pixelColor "white" = makeColor 1.0 1.0 1.0 1
pixelColor "green" = makeColor 0.2 0.7 0.2 1
pixelColor "dark green" = makeColor 0.1 0.4 0.1 1
pixelColor "purple" = makeColor 0.6 0.2 0.8 1
pixelColor "magenta" = makeColor 0.8 0.2 0.6 1
pixelColor "bone" = makeColor 0.9 0.9 0.8 1
pixelColor "iron gray" = makeColor 0.4 0.4 0.45 1
pixelColor _ = makeColor 0.5 0.5 0.5 1  -- Default gray

-- Helper to get color from palette list
getPaletteColor :: [String] -> Int -> Color
getPaletteColor palette idx = pixelColor (palette !! (idx `mod` length palette))

-- ============================================================================
-- Pixel Art Rendering Helpers
-- ============================================================================

-- Draw a pixel (small square) at position
drawPixel :: Float -> Float -> Float -> Color -> Picture
drawPixel x y size col = translate x y $ color col $ rectangleSolid size size

-- Draw pixel art sprite with chunky outline
drawPixelSprite :: Float -> Color -> [(Float, Float, String)] -> Picture
drawPixelSprite pixelSize outlineColor pixels = pictures
  [ -- Outline first (darker, slightly larger)
    pictures $ map (\(x, y, _) -> drawPixel x y (pixelSize * 1.1) outlineColor) pixels
  , -- Then main pixels
    pictures $ map (\(x, y, col) -> drawPixel x y pixelSize (pixelColor col)) pixels
               ]

-- ============================================================================
-- Tower Pixel Art Rendering
-- ============================================================================

renderPixelTower :: TowerType -> AnimationType -> Float -> Picture
renderPixelTower ArrowTower AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base: 8x8 grid = 8 pixels per grid unit
      -- "wooden tower with small pixel archer on top" - JSON spec
      pixels = [ -- Base foundation (wooden)
                 (-3, -4, "dark brown"), (-2, -4, "brown"), (-1, -4, "dark brown"), (0, -4, "brown"), (1, -4, "dark brown"), (2, -4, "brown"), (3, -4, "dark brown")
               , (-3, -3, "brown"), (-2, -3, "dark brown"), (-1, -3, "brown"), (0, -3, "dark brown"), (1, -3, "brown"), (2, -3, "dark brown"), (3, -3, "brown")
               -- Tower body (wooden)
               , (-2, -2, "brown"), (-1, -2, "tan"), (0, -2, "brown"), (1, -2, "tan"), (2, -2, "brown")
               , (-2, -1, "tan"), (-1, -1, "brown"), (0, -1, "tan"), (1, -1, "brown"), (2, -1, "tan")
               , (-2, 0, "brown"), (-1, 0, "tan"), (0, 0, "brown"), (1, 0, "tan"), (2, 0, "brown")
               -- Platform
               , (-3, 1, "brown"), (-2, 1, "tan"), (-1, 1, "brown"), (0, 1, "tan"), (1, 1, "brown"), (2, 1, "tan"), (3, 1, "brown")
               -- Small pixel archer on top
               , (0, 2, "tan")  -- Body
               , (-1, 3, "brown"), (0, 3, "tan"), (1, 3, "brown")  -- Head and shoulders
               , (0, 4, "brown")  -- Helmet
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower ArrowTower AnimAttack size =
  let pixelSize = size / 8
      pixels = [ -- Base foundation
                 (-3, -4, "dark brown"), (-2, -4, "brown"), (-1, -4, "dark brown"), (0, -4, "brown"), (1, -4, "dark brown"), (2, -4, "brown"), (3, -4, "dark brown")
               , (-3, -3, "brown"), (-2, -3, "dark brown"), (-1, -3, "brown"), (0, -3, "dark brown"), (1, -3, "brown"), (2, -3, "dark brown"), (3, -3, "brown")
               -- Tower body
               , (-2, -2, "brown"), (-1, -2, "tan"), (0, -2, "brown"), (1, -2, "tan"), (2, -2, "brown")
               , (-2, -1, "tan"), (-1, -1, "brown"), (0, -1, "tan"), (1, -1, "brown"), (2, -1, "tan")
               , (-2, 0, "brown"), (-1, 0, "tan"), (0, 0, "brown"), (1, 0, "tan"), (2, 0, "brown")
               -- Platform
               , (-3, 1, "brown"), (-2, 1, "tan"), (-1, 1, "brown"), (0, 1, "tan"), (1, 1, "brown"), (2, 1, "tan"), (3, 1, "brown")
               -- Archer shooting
               , (0, 2, "tan")  -- Body
               , (-1, 3, "brown"), (0, 3, "tan"), (1, 3, "brown")  -- Head and shoulders
               , (0, 4, "brown")  -- Helmet
               , (2, 3, "brown"), (3, 3, "brown")  -- Bow and arrow
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower CatapultTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ -- Base
                 (-4, -4, "dark gray"), (-3, -4, "gray"), (-2, -4, "dark gray"), (-1, -4, "gray"), (0, -4, "dark gray"), (1, -4, "gray"), (2, -4, "dark gray"), (3, -4, "gray"), (4, -4, "dark gray")
               , (-4, -3, "gray"), (-3, -3, "dark gray"), (-2, -3, "gray"), (-1, -3, "dark gray"), (0, -3, "gray"), (1, -3, "dark gray"), (2, -3, "gray"), (3, -3, "dark gray"), (4, -3, "gray")
               -- Catapult frame
               , (-3, -2, "brown"), (-2, -2, "dark brown"), (-1, -2, "brown"), (0, -2, "dark brown"), (1, -2, "brown"), (2, -2, "dark brown"), (3, -2, "brown")
               , (-3, -1, "dark brown"), (-2, -1, "brown"), (-1, -1, "dark brown"), (0, -1, "brown"), (1, -1, "dark brown"), (2, -1, "brown"), (3, -1, "dark brown")
               -- Arm
               , (-2, 0, "brown"), (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown"), (2, 0, "brown")
               , (-1, 1, "brown"), (0, 1, "dark brown"), (1, 1, "brown")
               , (0, 2, "brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower CatapultTower AnimAttack size =
  let pixelSize = size / 8
      pixels = [ (-3, -3, "gray"), (-1, -3, "gray"), (1, -3, "gray"), (3, -3, "gray")
               , (-3, -2, "dark gray"), (-1, -2, "gray"), (1, -2, "gray"), (3, -2, "dark gray")
               , (-2, -1, "brown"), (0, -1, "brown"), (2, -1, "brown")
               , (-1, 0, "brown"), (1, 0, "brown")
               , (0, 1, "brown"), (-1, 1, "brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower CrossbowTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ -- Base
                 (-3, -4, "dark brown"), (-2, -4, "brown"), (-1, -4, "dark brown"), (0, -4, "brown"), (1, -4, "dark brown"), (2, -4, "brown"), (3, -4, "dark brown")
               , (-3, -3, "brown"), (-2, -3, "dark brown"), (-1, -3, "brown"), (0, -3, "dark brown"), (1, -3, "brown"), (2, -3, "dark brown"), (3, -3, "brown")
               -- Tower body
               , (-2, -2, "brown"), (-1, -2, "tan"), (0, -2, "brown"), (1, -2, "tan"), (2, -2, "brown")
               , (-2, -1, "tan"), (-1, -1, "brown"), (0, -1, "tan"), (1, -1, "brown"), (2, -1, "tan")
               , (-2, 0, "brown"), (-1, 0, "tan"), (0, 0, "brown"), (1, 0, "tan"), (2, 0, "brown")
               -- Platform
               , (-3, 1, "brown"), (-2, 1, "tan"), (-1, 1, "brown"), (0, 1, "tan"), (1, 1, "brown"), (2, 1, "tan"), (3, 1, "brown")
               -- Crossbow
               , (-1, 2, "silver"), (0, 2, "brown"), (1, 2, "silver")
               , (0, 3, "brown")  -- Stock
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower FireTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ -- Base
                 (-3, -4, "dark gray"), (-2, -4, "gray"), (-1, -4, "dark gray"), (0, -4, "gray"), (1, -4, "dark gray"), (2, -4, "gray"), (3, -4, "dark gray")
               , (-3, -3, "gray"), (-2, -3, "dark gray"), (-1, -3, "gray"), (0, -3, "dark gray"), (1, -3, "gray"), (2, -3, "dark gray"), (3, -3, "gray")
               -- Tower body
               , (-2, -2, "dark gray"), (-1, -2, "gray"), (0, -2, "dark gray"), (1, -2, "gray"), (2, -2, "dark gray")
               , (-2, -1, "gray"), (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray"), (2, -1, "gray")
               , (-2, 0, "dark gray"), (-1, 0, "gray"), (0, 0, "dark gray"), (1, 0, "gray"), (2, 0, "dark gray")
               -- Fire brazier
               , (-1, 1, "dark gray"), (0, 1, "orange"), (1, 1, "dark gray")
               , (-1, 2, "orange"), (0, 2, "yellow"), (1, 2, "orange")
               , (0, 3, "red")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower TeslaTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ -- Base
                 (-3, -4, "dark gray"), (-2, -4, "gray"), (-1, -4, "dark gray"), (0, -4, "gray"), (1, -4, "dark gray"), (2, -4, "gray"), (3, -4, "dark gray")
               , (-3, -3, "gray"), (-2, -3, "dark gray"), (-1, -3, "gray"), (0, -3, "dark gray"), (1, -3, "gray"), (2, -3, "dark gray"), (3, -3, "gray")
               -- Tower body
               , (-2, -2, "gray"), (-1, -2, "dark gray"), (0, -2, "gray"), (1, -2, "dark gray"), (2, -2, "gray")
               , (-2, -1, "dark gray"), (-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (2, -1, "dark gray")
               , (-2, 0, "gray"), (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray"), (2, 0, "gray")
               -- Tesla coil
               , (-1, 1, "blue"), (0, 1, "light blue"), (1, 1, "blue")
               , (0, 2, "white")
               , (-1, 3, "blue"), (0, 3, "light blue"), (1, 3, "blue")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower BallistaTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ (-1, -3, "brown"), (0, -3, "brown"), (1, -3, "brown")
               , (-1, -2, "dark brown"), (0, -2, "silver"), (1, -2, "dark brown")
               , (-1, -1, "brown"), (0, -1, "silver"), (1, -1, "brown")
               , (-1, 0, "brown"), (0, 0, "silver"), (1, 0, "brown")
               , (-1, 1, "brown"), (0, 1, "brown"), (1, 1, "brown")
               , (0, 2, "brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower PoisonTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ (-2, -3, "green"), (0, -3, "green"), (2, -3, "green")
               , (-2, -2, "dark green"), (0, -2, "green"), (2, -2, "dark green")
               , (-1, -1, "brown"), (0, -1, "green"), (1, -1, "brown")
               , (-1, 0, "brown"), (0, 0, "dark green"), (1, 0, "brown")
               , (-1, 1, "brown"), (0, 1, "green"), (1, 1, "brown")
               , (0, 2, "green")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTower BombardTower AnimIdle size =
  let pixelSize = size / 8
      pixels = [ -- Base
                 (-4, -4, "dark gray"), (-3, -4, "gray"), (-2, -4, "dark gray"), (-1, -4, "gray"), (0, -4, "dark gray"), (1, -4, "gray"), (2, -4, "dark gray"), (3, -4, "gray"), (4, -4, "dark gray")
               , (-4, -3, "gray"), (-3, -3, "dark gray"), (-2, -3, "gray"), (-1, -3, "dark gray"), (0, -3, "gray"), (1, -3, "dark gray"), (2, -3, "gray"), (3, -3, "dark gray"), (4, -3, "gray")
               -- Cannon base
               , (-3, -2, "gray"), (-2, -2, "dark gray"), (-1, -2, "gray"), (0, -2, "dark gray"), (1, -2, "gray"), (2, -2, "dark gray"), (3, -2, "gray")
               , (-3, -1, "dark gray"), (-2, -1, "gray"), (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray"), (2, -1, "gray"), (3, -1, "dark gray")
               -- Cannon barrel
               , (-2, 0, "black"), (-1, 0, "dark gray"), (0, 0, "black"), (1, 0, "dark gray"), (2, 0, "black")
               , (-1, 1, "black"), (0, 1, "dark gray"), (1, 1, "black")
               , (0, 2, "black")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Death animation (collapsing)
renderPixelTower towerType AnimDeath size =
  let pixelSize = size / 8
      pixels = [ (-1, -1, "gray"), (0, -1, "gray"), (1, -1, "gray")
               , (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray")
               , (-1, 1, "dark gray"), (0, 1, "dark gray"), (1, 1, "dark gray")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Default for other tower types
renderPixelTower _ _ size = color (pixelColor "gray") $ circleSolid (size / 2)

-- ============================================================================
-- Enemy Pixel Art Rendering
-- ============================================================================

renderPixelEnemy :: UnitType -> AnimationType -> Float -> Picture
renderPixelEnemy GruntRaider AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base: 8x8 grid for detailed sprite
      -- "small humanoid in leather armor holding rusted axe" - JSON spec
      pixels = [ -- Head
                 (0, -3, "tan")  -- Face
               , (-1, -4, "brown"), (0, -4, "brown"), (1, -4, "brown")  -- Leather helmet
               -- Body/chest (leather armor)
               , (-1, -2, "brown"), (0, -2, "tan"), (1, -2, "brown")
               , (-2, -1, "brown"), (-1, -1, "tan"), (0, -1, "brown"), (1, -1, "tan"), (2, -1, "brown")
               -- Waist/belt
               , (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown")
               -- Legs
               , (-1, 1, "orange"), (0, 1, "dark brown"), (1, 1, "orange")
               , (-1, 2, "orange"), (0, 2, "dark brown"), (1, 2, "orange")
               -- Rusted axe (brown/orange colors)
               , (0, 3, "dark brown"), (0, 4, "brown"), (1, 3, "orange")  -- Axe head and handle
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy GruntRaider AnimMove size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Head
                 (0, -3, "tan")
               , (-1, -4, "brown"), (0, -4, "brown"), (1, -4, "brown")
               -- Body (slightly forward)
               , (-1, -2, "brown"), (0, -2, "tan"), (1, -2, "brown")
               , (-2, -1, "brown"), (-1, -1, "tan"), (0, -1, "brown"), (1, -1, "tan"), (2, -1, "brown")
               -- Waist
               , (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown")
               -- Running legs (spread)
               , (-2, 1, "orange"), (-1, 1, "dark brown"), (1, 1, "dark brown"), (2, 1, "orange")
               , (-2, 2, "orange"), (2, 2, "orange")
               -- Weapon (held back)
               , (-1, 3, "silver"), (-1, 4, "brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy GruntRaider AnimAttack size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Head
                 (0, -3, "tan")
               , (-1, -4, "brown"), (0, -4, "brown"), (1, -4, "brown")
               -- Body (leaning forward)
               , (-1, -2, "brown"), (0, -2, "tan"), (1, -2, "brown")
               , (-2, -1, "brown"), (-1, -1, "tan"), (0, -1, "brown"), (1, -1, "tan"), (2, -1, "brown")
               -- Waist
               , (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown")
               -- Legs (braced)
               , (-1, 1, "orange"), (0, 1, "dark brown"), (1, 1, "orange")
               , (-1, 2, "orange"), (1, 2, "orange")
               -- Swinging weapon
               , (-2, 2, "silver"), (-1, 2, "brown"), (0, 2, "brown"), (1, 2, "brown")
               , (-2, 3, "silver"), (-1, 3, "brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy BruteCrusher AnimIdle size =
  let pixelSize = size / 10  -- Larger grid for heavy unit
      -- "large orc brute with spiked club" - JSON spec
      pixels = [ -- Head (orc)
                 (0, -4, "green")  -- Top of head
               , (-1, -3, "green"), (0, -3, "dark green"), (1, -3, "green")
               , (-1, -2, "dark green"), (0, -2, "green"), (1, -2, "dark green")
               -- Shoulders/chest (large orc body)
               , (-3, -1, "green"), (-2, -1, "dark green"), (-1, -1, "green"), (0, -1, "dark green"), (1, -1, "green"), (2, -1, "dark green"), (3, -1, "green")
               -- Body
               , (-2, 0, "green"), (-1, 0, "dark green"), (0, 0, "brown"), (1, 0, "dark green"), (2, 0, "green")
               , (-2, 1, "green"), (-1, 1, "dark green"), (0, 1, "brown"), (1, 1, "dark green"), (2, 1, "green")
               -- Legs
               , (-1, 2, "green"), (0, 2, "brown"), (1, 2, "green")
               , (-1, 3, "green"), (0, 3, "brown"), (1, 3, "green")
               -- Spiked club (brown with spikes)
               , (0, 4, "brown"), (0, 5, "dark brown"), (0, 6, "brown")
               , (-1, 5, "brown"), (1, 5, "brown"), (-1, 6, "brown"), (1, 6, "brown")  -- Spiked club head
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy Direwolf AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Head
                 (0, -3, "dark gray")  -- Snout
               , (-1, -2, "dark gray"), (0, -2, "gray"), (1, -2, "dark gray")
               , (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray")
               -- Body
               , (-2, 0, "dark gray"), (-1, 0, "gray"), (0, 0, "dark gray"), (1, 0, "gray"), (2, 0, "dark gray")
               , (-2, 1, "gray"), (-1, 1, "dark gray"), (0, 1, "gray"), (1, 1, "dark gray"), (2, 1, "gray")
               -- Legs
               , (-2, 2, "blue"), (-1, 2, "dark gray"), (1, 2, "dark gray"), (2, 2, "blue")
               , (-2, 3, "blue"), (2, 3, "blue")
               -- Tail
               , (2, 0, "dark gray"), (3, 0, "gray")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy Shieldbearer AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Head
                 (0, -3, "silver")  -- Helmet top
               , (-1, -2, "silver"), (0, -2, "tan"), (1, -2, "silver")
               -- Shield (large)
               , (-3, -1, "silver"), (-2, -1, "blue"), (-1, -1, "silver"), (0, -1, "blue"), (1, -1, "silver"), (2, -1, "blue"), (3, -1, "silver")
               , (-3, 0, "silver"), (-2, 0, "blue"), (-1, 0, "silver"), (0, 0, "blue"), (1, 0, "silver"), (2, 0, "blue"), (3, 0, "silver")
               , (-3, 1, "silver"), (-2, 1, "blue"), (-1, 1, "silver"), (0, 1, "blue"), (1, 1, "silver"), (2, 1, "blue"), (3, 1, "silver")
               -- Body
               , (0, 0, "dark gray")
               -- Legs
               , (-1, 2, "silver"), (0, 2, "dark gray"), (1, 2, "silver")
               , (-1, 3, "silver"), (1, 3, "silver")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy Pyromancer AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      -- "hooded mage with fire staff" - JSON spec
      pixels = [ -- Hood
                 (0, -3, "red")  -- Top of hood
               , (-1, -2, "red"), (0, -2, "orange"), (1, -2, "red")
               , (-1, -1, "red"), (0, -1, "orange"), (1, -1, "red")
               -- Body (robes)
               , (-1, 0, "brown"), (0, 0, "red"), (1, 0, "brown")
               , (-1, 1, "brown"), (0, 1, "red"), (1, 1, "brown")
               -- Fire staff
               , (0, 2, "orange"), (0, 3, "red"), (0, 4, "orange")  -- Staff with fire
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy Necromancer AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      -- "dark robed figure with skull mask and purple aura" - JSON spec
      pixels = [ -- Skull mask
                 (0, -3, "bone")  -- Skull top
               , (-1, -2, "black"), (0, -2, "bone"), (1, -2, "black")
               , (-1, -1, "black"), (0, -1, "bone"), (1, -1, "black")
               -- Dark robes
               , (-1, 0, "black"), (0, 0, "purple"), (1, 0, "black")
               , (-1, 1, "black"), (0, 1, "purple"), (1, 1, "black")
               -- Purple aura and staff
               , (-2, 0, "purple"), (2, 0, "purple")  -- Aura
               , (0, 2, "purple"), (0, 3, "bone")  -- Staff
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy BoulderRamCrew AnimIdle size =
  let pixelSize = size / 10  -- Larger for siege unit
      -- "troop pushing wooden battering ram" - JSON spec
      pixels = [ -- Crew (troop)
                 (-2, -2, "brown"), (0, -2, "brown"), (2, -2, "brown")  -- Crew heads
               , (-2, -1, "tan"), (0, -1, "tan"), (2, -1, "tan")  -- Crew bodies
               -- Wooden battering ram
               , (-3, 0, "dark gray"), (-2, 0, "brown"), (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown"), (2, 0, "brown"), (3, 0, "dark gray")  -- Ram body
               , (-1, 1, "brown"), (0, 1, "brown"), (1, 1, "brown")  -- Ram base
               , (-1, 2, "tan"), (0, 2, "tan"), (1, 2, "tan")  -- Crew legs
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Death animation (falling)
renderPixelEnemy _ AnimDeath size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray")
               , (-1, 0, "gray"), (0, 0, "dark gray"), (1, 0, "gray")
               , (-1, 1, "dark gray"), (0, 1, "gray"), (1, 1, "dark gray")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Boss enemies
renderPixelEnemy IronbackMinotaur AnimIdle size =
  let pixelSize = size / 12  -- 128x128 boss: larger grid for detail
      pixels = [ -- Head
                 (0, -5, "dark brown")  -- Top of head
               , (-1, -4, "dark brown"), (0, -4, "red"), (1, -4, "dark brown")
               , (-1, -3, "dark brown"), (0, -3, "red"), (1, -3, "dark brown")
               -- Horns
               , (-2, -4, "dark brown"), (2, -4, "dark brown")
               , (-2, -3, "dark brown"), (2, -3, "dark brown")
               -- Shoulders/chest with iron plates
               , (-4, -2, "iron gray"), (-3, -2, "dark brown"), (-2, -2, "iron gray"), (-1, -2, "dark brown"), (0, -2, "red"), (1, -2, "dark brown"), (2, -2, "iron gray"), (3, -2, "dark brown"), (4, -2, "iron gray")
               , (-4, -1, "iron gray"), (-3, -1, "dark brown"), (-2, -1, "iron gray"), (-1, -1, "red"), (0, -1, "dark brown"), (1, -1, "red"), (2, -1, "iron gray"), (3, -1, "dark brown"), (4, -1, "iron gray")
               -- Body
               , (-4, 0, "iron gray"), (-3, 0, "dark brown"), (-2, 0, "iron gray"), (-1, 0, "red"), (0, 0, "dark brown"), (1, 0, "red"), (2, 0, "iron gray"), (3, 0, "dark brown"), (4, 0, "iron gray")
               , (-3, 1, "dark brown"), (-2, 1, "iron gray"), (-1, 1, "red"), (0, 1, "dark brown"), (1, 1, "red"), (2, 1, "iron gray"), (3, 1, "dark brown")
               -- Legs
               , (-2, 2, "dark brown"), (-1, 2, "red"), (0, 2, "dark brown"), (1, 2, "red"), (2, 2, "dark brown")
               , (-1, 3, "dark brown"), (0, 3, "red"), (1, 3, "dark brown")
               , (-1, 4, "dark brown"), (0, 4, "red"), (1, 4, "dark brown")
               -- Weapon
               , (0, 5, "brown"), (0, 6, "dark brown")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy FireDrake AnimIdle size =
  let pixelSize = size / 10
      pixels = [ (0, -4, "orange")  -- Head
               , (-1, -3, "orange"), (0, -3, "red"), (1, -3, "orange")
               , (-2, -2, "orange"), (-1, -2, "red"), (0, -2, "yellow"), (1, -2, "red"), (2, -2, "orange")
               , (-3, -1, "dark gray"), (-2, -1, "orange"), (-1, -1, "red"), (0, -1, "yellow"), (1, -1, "red"), (2, -1, "orange"), (3, -1, "dark gray")
               , (-3, 0, "dark gray"), (-2, 0, "orange"), (-1, 0, "red"), (0, 0, "yellow"), (1, 0, "red"), (2, 0, "orange"), (3, 0, "dark gray")
               , (-2, 1, "orange"), (-1, 1, "red"), (0, 1, "yellow"), (1, 1, "red"), (2, 1, "orange")
               , (-1, 2, "orange"), (0, 2, "red"), (1, 2, "orange")
               , (0, 3, "orange")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelEnemy LichKingArcthros AnimIdle size =
  let pixelSize = size / 10
      pixels = [ (0, -4, "blue")  -- Crown/head
               , (-1, -3, "blue"), (0, -3, "light blue"), (1, -3, "blue")
               , (-2, -2, "blue"), (-1, -2, "light blue"), (0, -2, "white"), (1, -2, "light blue"), (2, -2, "blue")
               , (-2, -1, "black"), (-1, -1, "blue"), (0, -1, "light blue"), (1, -1, "blue"), (2, -1, "black")
               , (-2, 0, "black"), (-1, 0, "blue"), (0, 0, "white"), (1, 0, "blue"), (2, 0, "black")
               , (-1, 1, "black"), (0, 1, "blue"), (1, 1, "black")
               , (-1, 2, "black"), (0, 2, "blue"), (1, 2, "black")
               , (0, 3, "blue")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Default for other enemy types
renderPixelEnemy _ _ size = color (pixelColor "gray") $ circleSolid (size / 2)

-- ============================================================================
-- Trap Pixel Art Rendering
-- ============================================================================

renderPixelTrap :: TrapType -> AnimationType -> Float -> Picture
renderPixelTrap SpikeTrap AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Trap base
                 (-3, -2, "dark gray"), (-2, -2, "gray"), (-1, -2, "dark gray"), (0, -2, "gray"), (1, -2, "dark gray"), (2, -2, "gray"), (3, -2, "dark gray")
               , (-3, -1, "gray"), (-2, -1, "dark gray"), (-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (2, -1, "dark gray"), (3, -1, "gray")
               , (-3, 0, "dark gray"), (-2, 0, "gray"), (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray"), (2, 0, "gray"), (3, 0, "dark gray")
               , (-3, 1, "gray"), (-2, 1, "dark gray"), (-1, 1, "gray"), (0, 1, "dark gray"), (1, 1, "gray"), (2, 1, "dark gray"), (3, 1, "gray")
               , (-3, 2, "dark gray"), (-2, 2, "gray"), (-1, 2, "dark gray"), (0, 2, "gray"), (1, 2, "dark gray"), (2, 2, "gray"), (3, 2, "dark gray")
               -- Hidden spikes (small)
               , (-1, 0, "silver"), (0, 0, "silver"), (1, 0, "silver")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTrap SpikeTrap AnimAttack size =
  let pixelSize = size / 6
      pixels = [ -- Trap base
                 (-3, -2, "dark gray"), (-2, -2, "gray"), (-1, -2, "dark gray"), (0, -2, "gray"), (1, -2, "dark gray"), (2, -2, "gray"), (3, -2, "dark gray")
               , (-3, -1, "gray"), (-2, -1, "dark gray"), (-1, -1, "gray"), (0, -1, "dark gray"), (1, -1, "gray"), (2, -1, "dark gray"), (3, -1, "gray")
               , (-3, 0, "dark gray"), (-2, 0, "gray"), (-1, 0, "dark gray"), (0, 0, "gray"), (1, 0, "dark gray"), (2, 0, "gray"), (3, 0, "dark gray")
               , (-3, 1, "gray"), (-2, 1, "dark gray"), (-1, 1, "gray"), (0, 1, "dark gray"), (1, 1, "gray"), (2, 1, "dark gray"), (3, 1, "gray")
               , (-3, 2, "dark gray"), (-2, 2, "gray"), (-1, 2, "dark gray"), (0, 2, "gray"), (1, 2, "dark gray"), (2, 2, "gray"), (3, 2, "dark gray")
               -- Extended spikes
               , (-1, -3, "silver"), (0, -3, "silver"), (1, -3, "silver")
               , (-1, -2, "silver"), (0, -2, "silver"), (1, -2, "silver")
               , (-1, -1, "silver"), (0, -1, "silver"), (1, -1, "silver")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTrap FreezeTrap AnimIdle size =
  let pixelSize = size / 6
      pixels = [ (-1, -1, "blue"), (0, -1, "light blue"), (1, -1, "blue")
               , (-1, 0, "light blue"), (0, 0, "white"), (1, 0, "light blue")
               , (-1, 1, "blue"), (0, 1, "light blue"), (1, 1, "blue")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTrap FirePitTrap AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Pit base
                 (-3, -2, "dark brown"), (-2, -2, "brown"), (-1, -2, "dark brown"), (0, -2, "brown"), (1, -2, "dark brown"), (2, -2, "brown"), (3, -2, "dark brown")
               , (-3, -1, "brown"), (-2, -1, "dark brown"), (-1, -1, "brown"), (0, -1, "dark brown"), (1, -1, "brown"), (2, -1, "dark brown"), (3, -1, "brown")
               , (-3, 0, "dark brown"), (-2, 0, "orange"), (-1, 0, "dark brown"), (0, 0, "yellow"), (1, 0, "dark brown"), (2, 0, "orange"), (3, 0, "dark brown")
               , (-3, 1, "brown"), (-2, 1, "dark brown"), (-1, 1, "brown"), (0, 1, "dark brown"), (1, 1, "brown"), (2, 1, "dark brown"), (3, 1, "brown")
               , (-3, 2, "dark brown"), (-2, 2, "brown"), (-1, 2, "dark brown"), (0, 2, "brown"), (1, 2, "dark brown"), (2, 2, "brown"), (3, 2, "dark brown")
               -- Fire glow
               , (-1, 0, "orange"), (0, 0, "yellow"), (1, 0, "orange")
               , (0, -1, "orange")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTrap MagicSnareTrap AnimIdle size =
  let pixelSize = size / 6
      pixels = [ (-1, -1, "purple"), (0, -1, "magenta"), (1, -1, "purple")
               , (-1, 0, "magenta"), (0, 0, "purple"), (1, 0, "magenta")
               , (-1, 1, "purple"), (0, 1, "magenta"), (1, 1, "purple")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

renderPixelTrap ExplosiveBarrel AnimIdle size =
  let pixelSize = size / 8  -- 64x64 base resolution
      pixels = [ -- Barrel body
                 (-3, -2, "brown"), (-2, -2, "dark brown"), (-1, -2, "brown"), (0, -2, "dark brown"), (1, -2, "brown"), (2, -2, "dark brown"), (3, -2, "brown")
               , (-3, -1, "dark brown"), (-2, -1, "brown"), (-1, -1, "dark brown"), (0, -1, "brown"), (1, -1, "dark brown"), (2, -1, "brown"), (3, -1, "dark brown")
               , (-3, 0, "brown"), (-2, 0, "dark brown"), (-1, 0, "brown"), (0, 0, "red"), (1, 0, "brown"), (2, 0, "dark brown"), (3, 0, "brown")
               , (-3, 1, "dark brown"), (-2, 1, "brown"), (-1, 1, "dark brown"), (0, 1, "brown"), (1, 1, "dark brown"), (2, 1, "brown"), (3, 1, "dark brown")
               , (-3, 2, "brown"), (-2, 2, "dark brown"), (-1, 2, "brown"), (0, 2, "dark brown"), (1, 2, "brown"), (2, 2, "dark brown"), (3, 2, "brown")
               -- Metal bands
               , (-3, -1, "silver"), (-2, -1, "silver"), (-1, -1, "silver"), (1, -1, "silver"), (2, -1, "silver"), (3, -1, "silver")
               , (-3, 1, "silver"), (-2, 1, "silver"), (-1, 1, "silver"), (1, 1, "silver"), (2, 1, "silver"), (3, 1, "silver")
               -- Fuse
               , (0, 3, "red"), (0, 4, "orange"), (0, 5, "yellow")
               ]
  in drawPixelSprite pixelSize (pixelColor "black") pixels

-- Default for other trap types
renderPixelTrap _ _ size = color (pixelColor "gray") $ circleSolid (size / 2)

-- AnimationType is now defined in Types.hs

-- Determine animation type based on enemy state
enemyStateToAnimation :: EnemyAIState -> AnimationType
enemyStateToAnimation Dead = AnimDeath
enemyStateToAnimation AttackingGate = AnimAttack
enemyStateToAnimation (AttackingTower _) = AnimAttack
enemyStateToAnimation AttackingCastle = AnimAttack
enemyStateToAnimation MovingToFort = AnimMove
enemyStateToAnimation InsideFort = AnimMove
enemyStateToAnimation (AttackingWall _) = AnimAttack
enemyStateToAnimation (ClimbingWall _) = AnimMove

-- Determine animation type for tower
towerStateToAnimation :: Float -> Float -> AnimationType
towerStateToAnimation lastFireTime currentTime =
  let timeSinceFire = currentTime - lastFireTime
  in if timeSinceFire < 0.5
     then AnimAttack
     else AnimIdle

-- ============================================================================
-- Environment Pixel Art Rendering (Walls, Fort, Castle)
-- ============================================================================

-- Render pixel art wall segment
renderPixelWall :: Float -> Float -> Float -> Float -> Float -> Picture
renderPixelWall x1 y1 x2 y2 healthRatio =
  let -- Calculate wall direction and length
      dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
      wallThickness = 20  -- Thicker walls
      pixelSize = 6
      
      -- Wall colors based on health
      wallColor = if healthRatio > 0.7 then "gray"
                  else if healthRatio > 0.4 then "dark gray"
                  else "black"
      
      -- Calculate perpendicular direction for thickness
      perpX = if len > 0 then -dy / len * wallThickness / 2 else 0
      perpY = if len > 0 then dx / len * wallThickness / 2 else 0
      
      -- Create wall pixels along the length with thickness
      numPixels = max 1 (floor (len / pixelSize))
      wallPixels = concatMap (\i ->
        let t = fromIntegral i / fromIntegral numPixels
            px = x1 + dx * t
            py = y1 + dy * t
            -- Create thickness by adding pixels perpendicular to wall
            thicknessPixels = map (\j ->
              let offset = (fromIntegral j - 2) * pixelSize / 2
                  offsetX = perpX * (1 + offset / wallThickness)
                  offsetY = perpY * (1 + offset / wallThickness)
              in (px + offsetX, py + offsetY, wallColor)
              ) [0..4]
        in thicknessPixels
        ) [0..numPixels-1]
      
      -- Add stone texture/details
      stonePixels = concatMap (\i ->
        let t = fromIntegral (i * 2) / fromIntegral numPixels
            px = x1 + dx * t
            py = y1 + dy * t
            detailColor = if i `mod` 2 == 0 then "dark gray" else "gray"
        in [(px + perpX * 1.2, py + perpY * 1.2, detailColor), (px - perpX * 1.2, py - perpY * 1.2, detailColor)]
        ) [0..(numPixels `div` 2)]
      
  in pictures $ map (\(x, y, col) -> translate x y $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) (wallPixels ++ stonePixels)

-- Render pixel art gate - more realistic medieval gate
renderPixelGate :: Float -> Float -> Float -> Float -> Bool -> Picture
renderPixelGate x y width height destroyed =
  let pixelSize = 8  -- Larger pixels for more detail
      gatePixels = if destroyed
        then [ -- Destroyed gate - rubble
               (-width/2, -height/2, "dark gray"), (-width/4, -height/2, "gray"), (0, -height/2, "black"), (width/4, -height/2, "gray"), (width/2, -height/2, "dark gray")
             , (-width/2, -height/4, "gray"), (-width/4, -height/4, "black"), (0, -height/4, "dark gray"), (width/4, -height/4, "black"), (width/2, -height/4, "gray")
             , (-width/2, 0, "dark gray"), (-width/4, 0, "black"), (0, 0, "gray"), (width/4, 0, "black"), (width/2, 0, "dark gray")
             , (-width/2, height/4, "gray"), (-width/4, height/4, "black"), (0, height/4, "dark gray"), (width/4, height/4, "black"), (width/2, height/4, "gray")
             , (-width/2, height/2, "dark gray"), (-width/4, height/2, "gray"), (0, height/2, "black"), (width/4, height/2, "gray"), (width/2, height/2, "dark gray")
             ]
        else [ -- Realistic medieval gate with wooden planks, iron bands, and decorative elements
               -- Top decorative arch/header
               (-width/2, -height/2, "dark gray"), (-width/3, -height/2, "gray"), (-width/6, -height/2, "dark gray"), (0, -height/2, "gray"), (width/6, -height/2, "dark gray"), (width/3, -height/2, "gray"), (width/2, -height/2, "dark gray")
               , (-width/3, -height/2 - 2, "gray"), (-width/6, -height/2 - 2, "dark gray"), (0, -height/2 - 2, "gray"), (width/6, -height/2 - 2, "dark gray"), (width/3, -height/2 - 2, "gray")
               -- Wooden planks (vertical)
               , (-width/2, -height/3, "brown"), (-width/2, -height/6, "dark brown"), (-width/2, 0, "brown"), (-width/2, height/6, "dark brown"), (-width/2, height/3, "brown")
               , (-width/3, -height/3, "dark brown"), (-width/3, -height/6, "brown"), (-width/3, 0, "dark brown"), (-width/3, height/6, "brown"), (-width/3, height/3, "dark brown")
               , (-width/6, -height/3, "brown"), (-width/6, -height/6, "dark brown"), (-width/6, 0, "brown"), (-width/6, height/6, "dark brown"), (-width/6, height/3, "brown")
               , (0, -height/3, "dark brown"), (0, -height/6, "brown"), (0, 0, "dark brown"), (0, height/6, "brown"), (0, height/3, "dark brown")
               , (width/6, -height/3, "brown"), (width/6, -height/6, "dark brown"), (width/6, 0, "brown"), (width/6, height/6, "dark brown"), (width/6, height/3, "brown")
               , (width/3, -height/3, "dark brown"), (width/3, -height/6, "brown"), (width/3, 0, "dark brown"), (width/3, height/6, "brown"), (width/3, height/3, "dark brown")
               , (width/2, -height/3, "brown"), (width/2, -height/6, "dark brown"), (width/2, 0, "brown"), (width/2, height/6, "dark brown"), (width/2, height/3, "brown")
               -- Iron bands (horizontal reinforcement)
               , (-width/2, -height/4, "silver"), (-width/3, -height/4, "iron gray"), (-width/6, -height/4, "silver"), (0, -height/4, "iron gray"), (width/6, -height/4, "silver"), (width/3, -height/4, "iron gray"), (width/2, -height/4, "silver")
               , (-width/2, 0, "iron gray"), (-width/3, 0, "silver"), (-width/6, 0, "iron gray"), (0, 0, "silver"), (width/6, 0, "iron gray"), (width/3, 0, "silver"), (width/2, 0, "iron gray")
               , (-width/2, height/4, "silver"), (-width/3, height/4, "iron gray"), (-width/6, height/4, "silver"), (0, height/4, "iron gray"), (width/6, height/4, "silver"), (width/3, height/4, "iron gray"), (width/2, height/4, "silver")
               -- Decorative studs/nails
               , (-width/3, -height/4, "black"), (0, -height/4, "black"), (width/3, -height/4, "black")
               , (-width/3, 0, "black"), (0, 0, "black"), (width/3, 0, "black")
               , (-width/3, height/4, "black"), (0, height/4, "black"), (width/3, height/4, "black")
               -- Bottom threshold
               , (-width/2, height/2, "dark gray"), (-width/3, height/2, "gray"), (-width/6, height/2, "dark gray"), (0, height/2, "gray"), (width/6, height/2, "dark gray"), (width/3, height/2, "gray"), (width/2, height/2, "dark gray")
               ]
  in translate x y $ pictures $ map (\(px, py, col) -> translate px py $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) gatePixels

-- Render pixel art fort (interior area)
renderPixelFort :: Float -> Float -> Float -> Float -> Picture
renderPixelFort centerX centerY width height =
  let pixelSize = 12
      -- Create tiled cobblestone pattern covering entire fort area
      tilesX = floor (width / pixelSize)
      tilesY = floor (height / pixelSize)
      fortPixels = concatMap (\iy ->
        concatMap (\ix ->
          let tileX = centerX - width/2 + fromIntegral ix * pixelSize + pixelSize/2
              tileY = centerY - height/2 + fromIntegral iy * pixelSize + pixelSize/2
              -- Alternating cobblestone pattern
              tileColor = if (ix + iy) `mod` 2 == 0 then "gray" else "dark gray"
          in [(tileX, tileY, tileColor)]
          ) [0..tilesX-1]
        ) [0..tilesY-1]
  in pictures $ map (\(px, py, col) -> translate px py $ color (pixelColor col) $ rectangleSolid pixelSize pixelSize) fortPixels

-- Render pixel art castle - FRONT VIEW (not top-down)
renderPixelCastle :: Float -> Float -> Float -> Picture
renderPixelCastle x y size =
  let pixelSize = size / 14  -- Grid for front view
      -- Front-facing medieval castle: towers on sides, gate in center, battlements on top
      -- Y increases upward, X is horizontal
      castlePixels = [ -- Foundation/base (bottom row, ground level)
                       (-6, -6, "dark gray"), (-5, -6, "gray"), (-4, -6, "dark gray"), (-3, -6, "gray"), (-2, -6, "dark gray"), (-1, -6, "gray"), (0, -6, "dark gray"), (1, -6, "gray"), (2, -6, "dark gray"), (3, -6, "gray"), (4, -6, "dark gray"), (5, -6, "gray"), (6, -6, "dark gray")
                     , (-6, -5, "gray"), (-5, -5, "dark gray"), (-4, -5, "gray"), (-3, -5, "dark gray"), (-2, -5, "gray"), (-1, -5, "dark gray"), (0, -5, "gray"), (1, -5, "dark gray"), (2, -5, "gray"), (3, -5, "dark gray"), (4, -5, "gray"), (5, -5, "dark gray"), (6, -5, "gray")
                     -- Left tower base
                     , (-6, -4, "dark gray"), (-5, -4, "gray"), (-4, -4, "dark gray")
                     , (-6, -3, "gray"), (-5, -3, "dark gray"), (-4, -3, "gray")
                     -- Left tower body (vertical)
                     , (-5, -2, "gray"), (-5, -1, "dark gray"), (-5, 0, "gray"), (-5, 1, "dark gray"), (-5, 2, "gray"), (-5, 3, "dark gray"), (-5, 4, "gray"), (-5, 5, "dark gray")
                     , (-6, -2, "dark gray"), (-4, -2, "dark gray"), (-6, -1, "gray"), (-4, -1, "gray"), (-6, 0, "dark gray"), (-4, 0, "dark gray"), (-6, 1, "gray"), (-4, 1, "gray"), (-6, 2, "dark gray"), (-4, 2, "dark gray"), (-6, 3, "gray"), (-4, 3, "gray"), (-6, 4, "dark gray"), (-4, 4, "dark gray"), (-6, 5, "gray"), (-4, 5, "gray")
                     -- Left tower battlements
                     , (-5, 6, "dark gray"), (-6, 6, "gray"), (-4, 6, "gray")
                     -- Left tower conical roof
                     , (-5, 7, "gray"), (-6, 7, "dark gray"), (-4, 7, "dark gray")
                     , (-5, 8, "dark gray")
                     -- Center gate area (main keep)
                     , (-3, -4, "gray"), (-2, -4, "dark gray"), (-1, -4, "gray"), (0, -4, "dark gray"), (1, -4, "gray"), (2, -4, "dark gray"), (3, -4, "gray")
                     , (-3, -3, "dark gray"), (-2, -3, "gray"), (-1, -3, "dark gray"), (0, -3, "gray"), (1, -3, "dark gray"), (2, -3, "gray"), (3, -3, "dark gray")
                     -- Main keep walls
                     , (-3, -2, "gray"), (-2, -2, "dark gray"), (-1, -2, "gray"), (0, -2, "dark gray"), (1, -2, "gray"), (2, -2, "dark gray"), (3, -2, "gray")
                     , (-3, -1, "dark gray"), (-2, -1, "gray"), (-1, -1, "dark gray"), (0, -1, "gray"), (1, -1, "dark gray"), (2, -1, "gray"), (3, -1, "dark gray")
                     , (-3, 0, "gray"), (-2, 0, "dark gray"), (-1, 0, "gray"), (0, 0, "dark gray"), (1, 0, "gray"), (2, 0, "dark gray"), (3, 0, "gray")
                     , (-3, 1, "dark gray"), (-2, 1, "gray"), (-1, 1, "dark gray"), (0, 1, "gray"), (1, 1, "dark gray"), (2, 1, "gray"), (3, 1, "dark gray")
                     , (-3, 2, "gray"), (-2, 2, "dark gray"), (-1, 2, "gray"), (0, 2, "dark gray"), (1, 2, "gray"), (2, 2, "dark gray"), (3, 2, "gray")
                     , (-3, 3, "dark gray"), (-2, 3, "gray"), (-1, 3, "dark gray"), (0, 3, "gray"), (1, 3, "dark gray"), (2, 3, "gray"), (3, 3, "dark gray")
                     -- Main keep battlements
                     , (-3, 4, "gray"), (-2, 4, "dark gray"), (-1, 4, "gray"), (0, 4, "dark gray"), (1, 4, "gray"), (2, 4, "dark gray"), (3, 4, "gray")
                     -- Center tower (keep)
                     , (-1, 4, "gray"), (0, 4, "dark gray"), (1, 4, "gray")
                     , (-1, 5, "dark gray"), (0, 5, "gray"), (1, 5, "dark gray")
                     , (-1, 6, "gray"), (0, 6, "dark gray"), (1, 6, "gray")
                     , (-1, 7, "dark gray"), (0, 7, "gray"), (1, 7, "dark gray")
                     , (0, 8, "gray")  -- Conical roof peak
                     -- Right tower base
                     , (4, -4, "dark gray"), (5, -4, "gray"), (6, -4, "dark gray")
                     , (4, -3, "gray"), (5, -3, "dark gray"), (6, -3, "gray")
                     -- Right tower body (vertical)
                     , (5, -2, "gray"), (5, -1, "dark gray"), (5, 0, "gray"), (5, 1, "dark gray"), (5, 2, "gray"), (5, 3, "dark gray"), (5, 4, "gray"), (5, 5, "dark gray")
                     , (4, -2, "dark gray"), (6, -2, "dark gray"), (4, -1, "gray"), (6, -1, "gray"), (4, 0, "dark gray"), (6, 0, "dark gray"), (4, 1, "gray"), (6, 1, "gray"), (4, 2, "dark gray"), (6, 2, "dark gray"), (4, 3, "gray"), (6, 3, "gray"), (4, 4, "dark gray"), (6, 4, "dark gray"), (4, 5, "gray"), (6, 5, "gray")
                     -- Right tower battlements
                     , (5, 6, "dark gray"), (4, 6, "gray"), (6, 6, "gray")
                     -- Right tower conical roof
                     , (5, 7, "gray"), (4, 7, "dark gray"), (6, 7, "dark gray")
                     , (5, 8, "dark gray")
                     -- Gate (wooden door in center)
                     , (-2, -2, "brown"), (-1, -2, "dark brown"), (0, -2, "brown"), (1, -2, "dark brown"), (2, -2, "brown")
                     , (-2, -1, "dark brown"), (-1, -1, "brown"), (0, -1, "silver"), (1, -1, "brown"), (2, -1, "dark brown")
                     , (-2, 0, "brown"), (-1, 0, "dark brown"), (0, 0, "brown"), (1, 0, "dark brown"), (2, 0, "brown")
                     -- Windows (left and right of gate)
                     , (-4, 0, "light blue"), (-4, 1, "light blue")
                     , (4, 0, "light blue"), (4, 1, "light blue")
                     -- Window frames
                     , (-4, 0, "dark gray"), (-4, 1, "dark gray")
                     , (4, 0, "dark gray"), (4, 1, "dark gray")
                     -- Flags/banners
                     , (-5, 4, "red"), (-4, 4, "red")
                     , (4, 4, "red"), (5, 4, "red")
                     , (0, 5, "blue")
                     ]
  in translate x y $ drawPixelSprite pixelSize (pixelColor "black") castlePixels

