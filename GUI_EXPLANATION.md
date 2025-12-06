# GUI System Explanation

## Overview

Your game uses **Gloss** (a Haskell graphics library) to render everything. The GUI is split into multiple rendering modules that work together to create the final display.

## Rendering Architecture

### Main Rendering Pipeline (`Main.hs`)

The main render function combines all layers:

```haskell
render :: World -> Picture
render world = pictures
  [ renderWorld world      -- Game world (background, enemies, towers, etc.)
  , renderUI world         -- UI overlays (panels, buttons, health bars)
  , renderDebug world      -- Debug information (if enabled)
  , renderControls         -- Control hints at bottom
  ]
```

**Order matters!** Items listed later are drawn on top.

---

## UI Components (`RenderUI.hs`)

### 1. **Top Bar** (Wooden Plank Header)
- **Location**: Top of screen
- **Purpose**: Decorative header with metal rivets
- **Rendering**: `renderTopBar` - Creates a wooden plank pattern with decorative rivets

### 2. **Treasury Panel** (Gold Display)
- **Location**: Top-left (`panelX = -worldWidth/2 + 120`)
- **Shows**: Current gold amount
- **Components**:
  - Wooden panel background
  - Gold coin icon (8x8 pixel art)
  - Gold text display
- **Rendering**: `renderTreasuryPanel`

### 3. **Level Panel** (Parchment Display)
- **Location**: Below treasury panel
- **Shows**: Current LEVEL and WAVE numbers
- **Components**:
  - Parchment background (beige rectangle)
  - Shield icon (for level)
  - Wave icon (for wave)
  - Text labels
- **Rendering**: `renderLevelPanel`

### 4. **Castle HP Bar** (Top-Right)
- **Location**: Top-right (`barX = worldWidth/2 - 200`)
- **Shows**: Castle health with color coding:
  - **Green** (>70% health)
  - **Yellow** (30-70% health)
  - **Red** (<30% health)
- **Components**:
  - Stone border
  - Health fill bar
  - "CASTLE" label
  - HP value (current/max)
- **Rendering**: `renderCastleHPBar`

### 5. **Tower Hotbar** (Tower Selection Buttons)
- **Location**: Below level panel
- **Shows**: 5 tower types (Arrow, Ballista, Fire, Tesla, Bombard)
- **Components**:
  - Wooden button frames (60x60 pixels each)
  - Tower icons (16x16 pixel art, scaled)
  - Key labels ("4", "5", "6", "7", "8")
  - Highlight when selected (darker brown)
- **Rendering**: `renderTowerHotbar` → `renderTowerButton`

**Tower Icons**:
- **Arrow Tower (4)**: Brown/tan pixels
- **Ballista (5)**: Brown with silver
- **Fire Tower (6)**: Red/orange pixels
- **Tesla (7)**: Gray/blue/white pixels
- **Bombard (8)**: Gray/black pixels

### 6. **Trap Hotbar** (Trap Selection Buttons)
- **Location**: Below tower hotbar
- **Shows**: 5 trap types (Spike, Freeze, Fire Pit, Magic Snare, Explosive)
- **Components**: Same structure as tower hotbar
- **Key labels**: "Z", "X", "C", "V", "B"
- **Rendering**: `renderTrapHotbar` → `renderTrapButton`

**Trap Icons**:
- **Spike (Z)**: Gray/dark gray/silver X pattern
- **Freeze (X)**: Blue/light blue/white
- **Fire Pit (C)**: Orange/yellow/dark brown
- **Magic Snare (V)**: Purple/magenta
- **Explosive Barrel (B)**: Brown with red dot

### 7. **Build Mode Display** (Tooltip)
- **Location**: Bottom-right
- **Shows**: Current build mode and cost
- **Components**:
  - Parchment tooltip panel
  - Tower/trap name and cost
  - Green text if affordable, red if not
- **Rendering**: `renderBuildModeDisplay`

### 8. **Game Status** (Victory/Defeat/Paused)
- **Location**: Center of screen
- **Shows**: Game over states or pause screen
- **Rendering**: `renderGameStatus`

---

## World Rendering (`RenderWorld.hs`)

### Rendering Order (Bottom to Top):

1. **Background** (`renderBackground`)
   - Grass tiles (64x64 pixel art tiles)
   - Path tiles (brown, curved paths from spawn points)

2. **Paths** (`renderPaths`)
   - Currently empty (paths are in background)

3. **Decorations** (`renderDecorations`)
   - Trees, bushes, rocks
   - Scattered across the field

4. **Fort** (`renderFort`)
   - Fort interior ground
   - Walls (`renderWalls`)
   - Gate (`renderGate`) with health bar
   - Corner towers (decorative)

5. **Castle** (`renderCastle`)
   - Large pixel art castle
   - Health bar above it

6. **Deployment Preview** (`renderDeploymentPreview`)
   - Red arch showing tower range when placing
   - Green/red circle showing valid placement

7. **Traps** (`renderTraps`)
   - Animated trap sprites

8. **Towers** (`renderTowers`)
   - Animated tower sprites
   - Health bars
   - Level indicators

9. **Enemies** (`renderEnemies`)
   - Animated enemy sprites
   - Health bars
   - Hit flash effects

10. **Projectiles** (`renderProjectiles`)
    - Animated projectile sprites
    - White/yellow outlines for visibility

11. **Visual Effects** (`renderVisualEffects`)
    - Impact flashes
    - Explosions
    - Fire bursts
    - Damage numbers

---

## Color System

The game uses a **medieval pixel art color palette**:

### UI Colors (`RenderUI.hs`):
- `woodBrown` - Light brown for wooden panels
- `darkWoodBrown` - Dark brown for borders
- `stoneGray` - Gray for stone elements
- `parchment` - Beige for parchment backgrounds
- `metalSilver` - Silver for metal elements
- `goldColor` - Gold for currency display

### World Colors (`RenderWorld.hs`):
- `brown` / `darkBrown` - Wood and earth tones
- `stoneGray` / `darkStoneGray` - Stone structures
- `gold` / `darkGold` - Treasure elements
- `ironGray` / `darkIron` - Metal elements

### Pixel Art Colors (`PixelArt.hs`):
- Uses named colors like "brown", "tan", "silver", "blue", etc.
- Converted to actual RGB values via `pixelColor` function

---

## Coordinate System

- **Origin (0,0)**: Center of screen
- **X-axis**: Left (-) to Right (+)
- **Y-axis**: Bottom (-) to Top (+)
- **World dimensions**: Defined in `Constants.hs`
  - `worldWidth` / `worldHeight` - Total screen size
  - `worldLeft` / `worldRight` / `worldTop` / `worldBottom` - Boundaries

### UI Positioning:
- **Top-left**: Negative X, Positive Y
- **Top-right**: Positive X, Positive Y
- **Bottom-left**: Negative X, Negative Y
- **Bottom-right**: Positive X, Negative Y

---

## How Selection Works

### Input State (`Types.hs`):
```haskell
data InputState = InputState
  { buildMode :: BuildMode  -- Current build mode
  , mousePos :: Vec2         -- Mouse position
  , ...
  }
```

### Build Mode (`Types.hs`):
```haskell
data BuildMode
  = PlaceTower TowerType
  | PlaceTrap TrapType
  | UpgradeMode
  | NoBuild
```

### Visual Feedback:
1. **Button Highlight**: Selected buttons show darker brown (`darkWoodBrown`)
2. **Preview Arch**: Red arch shows tower range when placing
3. **Preview Circle**: Green = valid placement, Red = invalid

---

## Animation System

Sprites use animation states:
- **AnimationState**: Current frame, animation type, time accumulator
- **Animation Types**: Idle, Attack, Move, Death, Flying
- **Sprite Animation Module**: `SpriteAnimation.hs` handles frame updates

---

## Key Functions to Understand

### UI Rendering:
- `renderUI` - Main UI coordinator
- `renderTowerHotbar` - Tower buttons
- `renderTrapHotbar` - Trap buttons
- `renderCastleHPBar` - Health display

### World Rendering:
- `renderWorld` - Main world coordinator
- `renderBackground` - Grass and paths
- `renderFort` - Fort structure
- `renderEnemies` / `renderTowers` / `renderTraps` - Entity rendering

### Utilities:
- `renderHealthBar` - Reusable health bar component
- `pixelColor` - Converts color names to RGB values
- `pictures` - Gloss function to combine multiple pictures

---

## Customization Tips

### To Change UI Colors:
Edit color definitions in `RenderUI.hs`:
```haskell
woodBrown :: Color
woodBrown = pixelColor "brown"  -- Change this
```

### To Change Button Size:
Edit in `renderTowerButton` / `renderTrapButton`:
```haskell
buttonWidth = 60   -- Change these
buttonHeight = 60
```

### To Change Panel Positions:
Edit X/Y coordinates in each render function:
```haskell
panelX = -worldWidth/2 + 120  -- Adjust offset
panelY = worldHeight/2 - 80   -- Adjust offset
```

### To Add New UI Element:
1. Create render function in `RenderUI.hs`
2. Add to `renderUI` function's `pictures` list
3. Position using `translate` function

---

## Performance Notes

- **Layering**: Items drawn later appear on top
- **Caching**: Sprites are loaded once and cached
- **Batch Rendering**: `pictures` combines multiple elements efficiently
- **60 FPS**: Game runs at 60 frames per second

---

## Debug Mode

Press **F1** to toggle debug display (`RenderDebug.hs`):
- Shows entity IDs
- Shows collision boxes
- Shows pathfinding paths
- Shows other debug information

