# Setup Instructions

## ✅ API Key Configuration

Your API key has been saved to `.env` file (this file is git-ignored for security).

## 📦 Installation Steps

1. **Install Node.js** (if not already installed):
   - Visit: https://nodejs.org/
   - Download and install the LTS version
   - Verify installation:
     ```bash
     node --version
     npm --version
     ```

2. **Install dependencies:**
   ```bash
   npm install
   ```

3. **Test API connection:**
   ```bash
   npm run test-api
   ```
   
   This will generate a test sprite to verify everything works.

## 🚀 Generate Sprites

Once setup is complete:

```bash
# Generate all sprites (takes time and uses API credits)
npm run generate

# Or generate a single animation for testing
npm run generate:single towers arrow_tower idle
```

## 📊 What Will Be Generated

- **8 Towers** with idle, attack, death animations
- **5 Traps** with idle, attack, death animations
- **8 Enemies** with idle, attack, death, move animations
- **3 Bosses** with idle, attack, death, move animations
- **Environment** assets (castle, walls, gate, tiles)
- **Projectiles** (arrows, bolts, fireballs, etc.)

**Total: ~437 sprite frames**

## ⚠️ Important Notes

- **API Costs**: Generating all sprites will use OpenAI API credits
- **Time**: Full generation takes 1-2 hours (1 second delay between frames)
- **Rate Limits**: The script includes delays to avoid rate limits
- **Existing Files**: Scripts skip existing files, so you can safely re-run

## 🔧 Troubleshooting

**"node: command not found"**
→ Install Node.js from https://nodejs.org/

**"npm: command not found"**
→ Node.js installation includes npm, reinstall Node.js if needed

**"OPENAI_API_KEY not found"**
→ Check that `.env` file exists in project root

**API Errors**
→ Run `npm run test-api` to diagnose connection issues

