# Enable Music - Step by Step Instructions

## Quick Steps:

### 1. Install Homebrew (if you don't have it)
Open Terminal and run:
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
**Note:** This will ask for your password (admin access required)

### 2. Install SDL2_mixer
After Homebrew is installed, run:
```bash
brew install sdl2_mixer
```

### 3. Enable Music in Code
Edit `medieval-siege.cabal` file:

Find lines 57-59:
```haskell
  -- SDL2_mixer dependencies (uncomment after installing SDL2_mixer via Homebrew)
  -- build-depends:      sdl2 >= 2.5
  --                   , sdl2-mixer >= 1.2
```

Change to (remove the `--`):
```haskell
  build-depends:      sdl2 >= 2.5
                    , sdl2-mixer >= 1.2
```

### 4. Rebuild and Run
```bash
cabal build
cabal run medieval-siege
```

## Or Use the Setup Script:

```bash
./setup_music.sh
```

This will guide you through the process automatically.

## Current Status:

✅ Code is ready - Music system implemented
✅ Music file ready - `assets/music/background_music.mp3` is in place  
⏳ Waiting for SDL2_mixer installation

The game **will run without music** until SDL2_mixer is installed - no errors!

