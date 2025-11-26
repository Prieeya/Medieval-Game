# Node.js Setup Guide

## Issue: Node.js Not Found in PATH

If you have Node.js installed but it's not recognized, you need to add it to your PATH or restart your terminal.

## Quick Solutions

### Option 1: Restart Your Terminal
1. Close this terminal/PowerShell window
2. Open a new terminal/PowerShell window
3. Navigate back to: `cd C:\Users\Bivek\Medieval-Game`
4. Try: `node --version` and `npm --version`

### Option 2: Add Node.js to PATH Manually

1. **Find Node.js Installation**
   - Common locations:
     - `C:\Program Files\nodejs\`
     - `C:\Program Files (x86)\nodejs\`
     - `%LOCALAPPDATA%\Programs\nodejs\`

2. **Add to PATH**
   - Press `Win + R`, type `sysdm.cpl`, press Enter
   - Go to "Advanced" tab
   - Click "Environment Variables"
   - Under "System variables", find "Path" and click "Edit"
   - Click "New" and add the Node.js path (e.g., `C:\Program Files\nodejs`)
   - Click OK on all dialogs
   - **Restart your terminal**

### Option 3: Use Full Path

If you know where Node.js is installed, you can use the full path:

```powershell
# Example (adjust path as needed):
& "C:\Program Files\nodejs\npm.cmd" install
```

## Verify Installation

After adding to PATH or restarting terminal:

```bash
node --version
npm --version
```

Both should show version numbers.

## Once Node.js Works

Then run:
```bash
npm install
npm run test-hf
npm run generate:hf
```

## Alternative: Install Node.js

If Node.js isn't actually installed:

1. Download from: https://nodejs.org/
2. Install the LTS version
3. **Important**: Check "Add to PATH" during installation
4. Restart terminal after installation

