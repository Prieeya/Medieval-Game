# Quick Start Guide - Sprite Generation

## 🚀 Quick Setup (3 steps)

1. **Install dependencies:**
   ```bash
   npm install
   ```

2. **Set your API key:**
   ```bash
   export OPENAI_API_KEY="sk-..."
   ```

3. **Generate all sprites:**
   ```bash
   npm run generate
   ```

## 📋 What Gets Generated

- **8 Towers** × 3 animations (idle, attack, death) = ~120 frames
- **5 Traps** × 3 animations (idle, attack, death) = ~45 frames  
- **8 Enemies** × 4 animations (idle, attack, death, move) = ~160 frames
- **3 Bosses** × 4 animations (idle, attack, death, move) = ~80 frames
- **7 Environment** assets = ~20 frames
- **6 Projectiles** = ~12 frames

**Total: ~437 sprite frames**

## ⚡ Quick Commands

```bash
# Generate everything
npm run generate

# Generate one animation
npm run generate:single towers arrow_tower attack

# Check what's available
cat assetsConfigAnimated.mjs
```

## 📁 Output Location

All sprites saved to:
```
assets/images/
├── towers/arrow_tower/idle/frame_01.png
├── towers/arrow_tower/attack/frame_01.png
└── ...
```

## ⚙️ Configuration

- **Model**: Set `OPENAI_IMAGE_MODEL` env var (default: `dall-e-3`)
- **Prompts**: Edit `assetsConfigAnimated.mjs`
- **Frame counts**: Edit `assetsConfigAnimated.mjs`

## 💡 Tips

- Scripts skip existing files (safe to re-run)
- 1 second delay between API calls (rate limiting)
- Images auto-trimmed to remove transparent borders
- All images are 1024×1024 (scales to 64×64 in-game)

## 🐛 Troubleshooting

**"OPENAI_API_KEY not set"**
→ Export the environment variable or create `.env` file

**"Rate limit exceeded"**
→ Increase delay in script (line ~85 in generate_animated_sprites.mjs)

**"Model not found"**
→ Check if `gpt-image-1` exists, or use `dall-e-3` (default)

