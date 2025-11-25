# Sprite Generation Pipeline

This directory contains the automated sprite generation system for Medieval Siege Simulator.

## Setup

1. **Install Node.js dependencies:**
   ```bash
   npm install
   ```

2. **Set OpenAI API Key:**
   ```bash
   export OPENAI_API_KEY="your-api-key-here"
   ```
   
   Or create a `.env` file:
   ```
   OPENAI_API_KEY=your-api-key-here
   ```

3. **Optional: Set Image Model:**
   ```bash
   export OPENAI_IMAGE_MODEL="gpt-image-1"  # or "dall-e-3" (default)
   ```
   
   The default model is `dall-e-3`. If you have access to `gpt-image-1`, set the environment variable.

## Usage

### Generate All Sprites

Generate all animated sprites for all units:

```bash
npm run generate
# or
node generate_animated_sprites.mjs
```

This will:
- Generate all animations for all towers, traps, enemies, bosses, environment, and projectiles
- Save sprites to `assets/images/<category>/<name>/<animation>/frame_XX.png`
- Skip existing files (safe to re-run)
- Show progress for each asset

### Generate Single Animation

Generate a specific animation for a specific unit:

```bash
npm run generate:single towers arrow_tower attack
# or
node generate_single_animation.mjs towers arrow_tower attack
```

Arguments:
- `<category>`: `towers`, `traps`, `enemies`, `bosses`, `environment`, or `projectiles`
- `<name>`: The unit name (e.g., `arrow_tower`, `grunt_raider`)
- `<animation>`: `idle`, `attack`, `death`, or `move`

## Asset Structure

All sprites are saved in the following structure:

```
assets/images/
├── towers/
│   ├── arrow_tower/
│   │   ├── idle/
│   │   │   ├── frame_01.png
│   │   │   ├── frame_02.png
│   │   │   └── frame_03.png
│   │   ├── attack/
│   │   │   └── ...
│   │   └── death/
│   │       └── ...
│   └── ...
├── traps/
├── enemies/
├── bosses/
├── environment/
└── projectiles/
```

## Configuration

Edit `assetsConfigAnimated.mjs` to:
- Add new units
- Modify animation frame counts
- Update prompts for different art styles
- Change animation names

## Features

- ✅ **Automatic directory creation**
- ✅ **Transparent border trimming** (using sharp)
- ✅ **1024x1024 resolution** (scales down to 64x64 in-game)
- ✅ **Rate limiting** (1 second between API calls)
- ✅ **Skip existing files** (safe to re-run)
- ✅ **Progress logging**
- ✅ **Consistent art style** (via base style prompt)

## Art Direction

All sprites follow these guidelines:
- Top-down 2D orthographic view
- Realistic medieval style
- Consistent lighting and shadows
- Transparent backgrounds
- Clean silhouettes
- Readable at 64×64 after scaling

## Notes

- The script uses OpenAI's DALL-E 3 API (model: `dall-e-3`)
- Each frame is generated separately to maintain consistency
- Generated images are automatically trimmed to remove transparent borders
- The pipeline generates hundreds of sprites - expect it to take time and cost API credits

## Troubleshooting

**Error: OPENAI_API_KEY not set**
- Make sure you've exported the environment variable or created a `.env` file

**Error: Rate limit exceeded**
- The script includes 1-second delays between requests
- If you still hit limits, increase the delay in the script

**Sprites look inconsistent**
- Each frame is generated separately, so some variation is expected
- For better consistency, you may want to generate frames in batches and refine prompts

**Missing animations**
- Check `assetsConfigAnimated.mjs` to ensure all required animations are defined
- Some units don't have all animation types (e.g., towers don't have `move` animations)

