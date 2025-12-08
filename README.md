# Medieval Siege Defense

A tower defense game where you defend your castle against waves of enemies. Build towers, place traps, and use abilities to survive increasingly difficult waves.

## How to Play

Defend your castle by placing towers inside your fort and traps on the battlefield. Each level has 3 waves followed by a boss wave. Complete all 3 levels to win.

After each wave, you earn gold to build more defenses:
- Waves 1-2: Base gold
- Wave 3: Extra bonus gold before the boss

## Controls

**Building:**
- **4-8**: Towers (Arrow, Ballista, Fire, Tesla, Bombard)
- **9**: Catapult, **0**: Crossbow, **-**: Poison
- **Z-X-C-V-B**: Traps (Spike, Freeze, Fire Pit, Magic Snare, Explosive)
- **U**: Upgrade mode (click towers/traps to upgrade)
- **ESC**: Cancel

**Fort Management:**
- **G**: Repair destroyed gates (150 gold each)
- **H**: Upgrade all gates (250-550 gold, makes them permanently stronger)

**Abilities:**
- **Q**: Firestorm, **W**: Freeze Field, **E**: Repair Walls, **R**: Time Slow

**Other:**
- **Space**: Pause
- **1/2/3**: Game speed
- **TAB**: Shop menu
- **F1**: Debug mode

## Towers

- **Arrow** (4) - 98g - Basic rapid fire
- **Ballista** (5) - 290g - Long range, armor piercing
- **Fire** (6) - 115g - Damage over time
- **Tesla** (7) - 186g - Chain lightning
- **Bombard** (8) - 340g - Heavy AoE cannon
- **Catapult** (9) - 170g - AoE damage
- **Crossbow** (0) - 220g - High damage sniper
- **Poison** (-) - 90g - Debuff tower

All towers can be upgraded to level 3 for better stats and special abilities.

## Traps

- **Spike** (Z) - 20g - Instant damage
- **Freeze** (X) - 35g - Slows enemies
- **Fire Pit** (C) - 45g - Continuous fire damage
- **Magic Snare** (V) - 50g - Immobilizes enemies
- **Explosive Barrel** (B) - 80g - Burst damage

## Setup

**Prerequisites:**
- GHC 8.10+
- Cabal
- SDL2 and SDL2_mixer

**Install dependencies:**

macOS:
```bash
brew install sdl2 sdl2_mixer
```

Linux:
```bash
sudo apt-get install libsdl2-dev libsdl2-mixer-dev
```

**Build and run:**
```bash
cabal build
cabal run medieval-siege
```

For music support:
```bash
cabal build --flag use-sdl2-mixer
```

## Tips

- Place towers inside your fort for protection
- Use traps early - they're cheap and effective
- Save bonus gold from wave 3 for powerful towers
- Repair gates before boss waves
- The AI adapts to your strategy, so mix things up
- Upgrade key towers instead of building many weak ones

## Project Structure

```
src/
├── Main.hs                    # Entry point, window setup, game loop init
├── Types.hs                   # All game data types (World, Enemy, Tower, etc.)
├── Constants.hs               # Game constants (costs, stats, upgrades)
├── Config.hs                  # Initial game state setup
├── GameLoop.hs                # Main update loop, coordinates all systems
├── Assets.hs                  # Asset loading and management
│
├── AI/
│   ├── Director.hs            # Adaptive AI that plans enemy waves
│   ├── FSM.hs                 # Enemy AI state machine
│   ├── Pathfinding.hs         # Pathfinding algorithms
│   └── ThreatAnalysis.hs      # Analyzes player defenses
│
├── Input/
│   └── InputHandler.hs        # Keyboard and mouse input
│
├── Rendering/
│   ├── RenderWorld.hs         # Renders game world (towers, enemies)
│   ├── RenderUI.hs            # Renders UI (HP bars, buttons, menus)
│   ├── RenderDebug.hs         # Debug overlay (paths, ranges)
│   ├── PixelArt.hs            # Pixel art sprite rendering
│   ├── SpriteAnimation.hs     # Sprite animation system
│   └── RenderUtils.hs         # Shared rendering utilities
│
├── Systems/
│   ├── WaveSystem.hs          # Wave progression and spawn timing
│   ├── TowerSystem.hs         # Tower targeting and shooting
│   ├── TrapSystem.hs          # Trap triggering and damage
│   ├── DamageSystem.hs        # Damage calculations and armor
│   ├── AbilitySystem.hs       # Special abilities (firestorm, freeze)
│   ├── BossAbilities.hs       # Boss enemy special abilities
│   ├── ResourceSystem.hs      # Gold management
│   ├── FortSystem.hs          # Fort walls and gates logic
│   └── CastleSystem.hs        # Castle HP and defeat conditions
│
├── Physics/
│   ├── Collision.hs           # Collision detection
│   ├── PhysicsCore.hs         # Physics calculations (movement)
│   └── Projectiles.hs         # Projectile movement and hits
│
└── Audio/
    ├── Music.hs               # Background music system
    ├── SoundEvents.hs         # Sound effect events mapping
    └── AudioCore.hs           # Audio playback core
```

## Victory

Win by completing all 3 levels. Lose if your castle HP reaches 0.
