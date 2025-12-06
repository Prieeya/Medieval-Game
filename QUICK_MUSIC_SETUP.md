# Quick Music Setup Guide

## Option 1: Automatic Setup (Recommended)

Run the setup script:
```bash
./setup_music.sh
```

This will:
1. Check for Homebrew
2. Install SDL2_mixer
3. Guide you through enabling it in the cabal file

## Option 2: Manual Setup

### Step 1: Install Homebrew (if needed)
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Follow the on-screen instructions. You'll need to enter your password.

### Step 2: Install SDL2_mixer
```bash
brew install sdl2_mixer
```

### Step 3: Enable Music in Cabal File

Edit `medieval-siege.cabal` and find these lines around line 57:
```haskell
  -- SDL2_mixer dependencies (uncomment after installing SDL2_mixer via Homebrew)
  -- build-depends:      sdl2 >= 2.5
  --                   , sdl2-mixer >= 1.2
```

Remove the `--` to uncomment them:
```haskell
  build-depends:      sdl2 >= 2.5
                    , sdl2-mixer >= 1.2
```

### Step 4: Rebuild and Run
```bash
cabal build
cabal run medieval-siege
```

## Verify Installation

Check if SDL2_mixer is installed:
```bash
pkg-config --modversion SDL2_mixer
```

You should see a version number like `2.6.0` or similar.

## Current Status

✅ **Game runs without music** - All SDL2 code is optional
✅ **Music file ready** - `assets/music/background_music.mp3` is in place
⏳ **Waiting for SDL2_mixer** - Install it to enable music

Once SDL2_mixer is installed and the cabal file is updated, music will play automatically!

