# Sound Effects Directory

This directory contains sound effect files for the Medieval Siege Defense game.

## Required Sound Files

The game expects the following sound files (in WAV format):

### Tower Sounds
- `tower_arrow_fire.wav` - Arrow tower firing
- `tower_crossbow_fire.wav` - Crossbow tower firing
- `tower_ballista_fire.wav` - Ballista tower firing
- `tower_fire_fire.wav` - Fire tower firing
- `tower_lightning_fire.wav` - Lightning/Tesla tower firing
- `tower_catapult_fire.wav` - Catapult tower firing
- `tower_cannon_fire.wav` - Cannon/Bombard tower firing
- `tower_poison_fire.wav` - Poison tower firing
- `tower_built.wav` - Tower construction sound
- `tower_upgraded.wav` - Tower upgrade sound

### Projectile Sounds
- `projectile_arrow_hit.wav` - Arrow impact
- `projectile_bolt_hit.wav` - Ballista bolt impact
- `projectile_fireball_hit.wav` - Fireball impact
- `projectile_ice_hit.wav` - Ice shard impact
- `projectile_lightning_hit.wav` - Lightning bolt impact
- `projectile_rock_hit.wav` - Catapult rock impact
- `projectile_barrage_hit.wav` - Barrage shot impact

### Enemy Sounds
- `enemy_death.wav` - Enemy death sound
- `enemy_spawn.wav` - Enemy spawn sound

### Trap Sounds
- `trap_spike.wav` - Spike trap trigger
- `trap_freeze.wav` - Freeze trap trigger
- `trap_fire.wav` - Fire pit trap trigger
- `trap_magic.wav` - Magic snare trap trigger
- `trap_explosive.wav` - Explosive barrel trigger
- `trap_placed.wav` - Trap placement sound

### Structure Sounds
- `gate_hit.wav` - Gate taking damage
- `gate_destroyed.wav` - Gate destruction
- `castle_hit.wav` - Castle taking damage

### Ability Sounds
- `ability_firestorm.wav` - Firestorm ability
- `ability_freeze.wav` - Freeze field ability
- `ability_repair.wav` - Repair walls ability
- `ability_slow.wav` - Time slow ability

### Wave/Level Sounds
- `wave_start.wav` - Wave beginning
- `wave_complete.wav` - Wave completion
- `level_complete.wav` - Level completion

### Game State Sounds
- `victory.wav` - Victory music/sound
- `defeat.wav` - Defeat music/sound

### UI Sounds
- `ui_click.wav` - Button click
- `ui_coin.wav` - Coin collection
- `error.wav` - Error sound

## Audio Format

- **Format**: WAV (uncompressed or compressed)
- **Sample Rate**: 22050 Hz or 44100 Hz recommended
- **Channels**: Mono or Stereo
- **Bit Depth**: 16-bit recommended

## Getting Sound Files

You can:
1. Create your own sound effects
2. Use free sound libraries like:
   - Freesound.org (https://freesound.org)
   - OpenGameArt.org (https://opengameart.org)
   - Zapsplat (https://www.zapsplat.com)
   - Incompetech (https://incompetech.com)

## Note

If sound files are missing, the game will continue to run but without sound effects. The audio system will log warnings for missing files.

