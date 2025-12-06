# Installing SDL2_mixer for Background Music

The game needs SDL2_mixer library to play background music. Here's how to install it:

## macOS Installation

### Step 1: Install Homebrew (if not already installed)

Open Terminal and run:
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Follow the on-screen instructions.

### Step 2: Install SDL2_mixer

After Homebrew is installed, run:
```bash
brew install sdl2_mixer
```

### Step 3: Install pkg-config (if needed)

```bash
brew install pkg-config
```

### Step 4: Rebuild the game

```bash
cabal build
cabal run medieval-siege
```

## Alternative: Run Without Music

If you don't want to install SDL2_mixer, you can temporarily remove the music dependencies:

1. Edit `medieval-siege.cabal` and remove these lines:
   ```
   , sdl2 >= 2.5
   , sdl2-mixer >= 1.2
   ```

2. Edit `src/Main.hs` and comment out music-related code

3. The game will run without background music

## Verify Installation

After installing, verify with:
```bash
pkg-config --modversion SDL2_mixer
```

You should see a version number like `2.6.0` or similar.

