# Game Audio Setup

The game's code expects specific .wav files in this directory (`assets/sounds/`).
If files are missing, the game will run silently (no crash).

## Required Sound Files (WAV Format)

### Tower Sounds
- `tower_arrow_fire.wav`      (Arrow Tower shooting)
- `tower_catapult_fire.wav`   (Catapult firing)
- `tower_crossbow_fire.wav`   (Crossbow firing)
- `tower_fire_fire.wav`       (Fire Tower firing)
- `tower_tesla_fire.wav`      (Tesla/Lightning Tower firing)
- `tower_ballista_fire.wav`   (Ballista firing)
- `tower_poison_fire.wav`     (Poison Tower firing)
- `tower_bombard_fire.wav`    (Bombard firing)
- `build_tower.wav`           (Placing a tower)
- `sell_tower.wav`            (Selling a tower)
- `upgrade_tower.wav`         (Upgrading a tower)
- `structure_destroyed.wav`   (Tower destroyed)

### Combat & Impact
- `hit_arrow.wav`             (Arrow hitting enemy)
- `hit_heavy.wav`             (Heavy projectile hitting)
- `hit_fire.wav`              (Fire damage)
- `hit_ice.wav`               (Ice/Freeze damage)
- `hit_lightning.wav`         (Lightning damage)
- `hit_rock.wav`              (Rock/Catapult hit)
- `enemy_hit.wav`             (Generic enemy hit)
- `enemy_death.wav`           (Enemy dying)
- `enemy_attack.wav`          (Enemy attacking wall/gate)
- `trap_trigger.wav`          (Enemy stepping on trap)

### Fort & Structure
- `gate_hit.wav`              (Gate taking damage)
- `gate_destroyed.wav`        (Gate destroyed)
- `repair.wav`                (Gate repaired)
- `wall_hit.wav`              (Wall taking damage)
- `castle_hit.wav`            (Castle taking damage)

### UI & Gameplay
- `wave_start.wav`            (Wave started)
- `wave_complete.wav`         (Wave finished)
- `level_complete.wav`        (Level finished)
- `game_over.wav`             (Game Over / Defeat)
- `victory.wav`               (Victory)
- `ui_hover.wav`              (Mouse hover over button)
- `ui_click.wav`              (Button click)
- `ui_error.wav`              (Not enough gold / Invalid placement)
- `upgrade_unlock.wav`        (Unlocking items in Shop)

## Current Status
✅ **Real sound files have been generated!** All 36 required sound files exist with actual audio content (tones, impacts, fire sounds, etc.). These are programmatically generated sounds that will play in the game.

## Instructions (Optional - For Real Sound Effects)
If you want actual sound effects instead of silence:
1. Download sound effects from free sites (e.g., freesound.org, opengameart.org).
2. Rename them to match the filenames above exactly.
3. Replace the placeholder files in this folder.
4. Re-run the game.

**Note:** The placeholder files are valid WAV files (just silent), so the game works perfectly fine with them. Replacing them with real sounds is optional but recommended for better gameplay experience.
