#!/bin/bash

# Setup script for enabling background music in Medieval Siege Defense

echo "==================================="
echo "Medieval Siege Defense - Music Setup"
echo "==================================="
echo ""

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "❌ Homebrew is not installed."
    echo ""
    echo "To install Homebrew, run this command in Terminal:"
    echo '  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
    echo ""
    echo "After installing Homebrew, run this script again:"
    echo "  ./setup_music.sh"
    echo ""
    exit 1
fi

echo "✓ Homebrew is installed"
echo ""

# Install SDL2_mixer
echo "Installing SDL2_mixer..."
brew install sdl2_mixer

if [ $? -ne 0 ]; then
    echo ""
    echo "❌ Failed to install SDL2_mixer"
    echo "The game will run without music."
    exit 1
fi

echo ""
echo "✓ SDL2_mixer installed successfully!"
echo ""

# Update cabal file automatically
echo "Updating cabal file to enable music..."
CABAL_FILE="medieval-siege.cabal"

if [ -f "$CABAL_FILE" ]; then
    # Create backup
    cp "$CABAL_FILE" "$CABAL_FILE.backup"
    
    # Uncomment SDL2 dependencies
    sed -i '' 's/^  -- build-depends:      sdl2 >= 2.5$/  build-depends:      sdl2 >= 2.5/' "$CABAL_FILE"
    sed -i '' 's/^  --                   , sdl2-mixer >= 1.2$/                    , sdl2-mixer >= 1.2/' "$CABAL_FILE"
    
    echo "✓ Cabal file updated!"
    echo ""
    echo "Next steps:"
    echo "1. Run: cabal build"
    echo "2. Run: cabal run medieval-siege"
    echo ""
    echo "Music will now play automatically! 🎵"
else
    echo "❌ Could not find $CABAL_FILE"
    exit 1
fi
