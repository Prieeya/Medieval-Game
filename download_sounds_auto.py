#!/usr/bin/env python3
"""
Download free sound effects for the game.
Uses built-in Python libraries to download from free sources.
"""

import os
import urllib.request
import urllib.error
import time

os.makedirs("assets/sounds", exist_ok=True)

# Try to download from various free sources
# Note: Many sites block automated downloads, so we'll try multiple sources

download_attempts = []

# Try Kenney.nl game assets (they have free sound packs)
kenney_sounds = {
    "ui_click.wav": "https://kenney.nl/assets/audio-pack-sfx",
    # Kenney requires manual download, but we can try
}

# Try to download from public CDNs or repositories
# These are example URLs - we'll need to find actual working ones
public_sounds = {
    # Generic game sounds from public repositories
    # Note: These are placeholder URLs - we need to find actual working ones
}

def try_download(url, filename):
    """Try to download a file."""
    try:
        print(f"Trying to download {filename}...")
        filepath = os.path.join("assets/sounds", filename)
        
        # Set a user agent to avoid blocking
        req = urllib.request.Request(url)
        req.add_header('User-Agent', 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)')
        
        with urllib.request.urlopen(req, timeout=10) as response:
            data = response.read()
            with open(filepath, 'wb') as f:
                f.write(data)
        print(f"✓ Successfully downloaded: {filename}")
        return True
    except Exception as e:
        print(f"✗ Failed to download {filename}: {e}")
        return False

print("="*60)
print("SOUND DOWNLOAD SCRIPT")
print("="*60)
print("\nMost free sound sites require manual downloads.")
print("This script will attempt to find and download sounds,")
print("but you may need to manually download from:")
print("  - opengameart.org")
print("  - freesound.org (requires account)")
print("  - kenney.nl")
print("  - mixkit.co")
print("\nAttempting automated downloads...\n")

# Since direct automated downloads are limited, let's create a helper script
# that generates a download list with instructions

print("Creating download helper script...")

helper_script = """#!/bin/bash
# Sound Download Helper
# This script helps you download sounds manually

echo "Sound files needed for the game:"
echo ""
echo "Visit these sites and download appropriate sounds:"
echo ""
echo "1. OpenGameArt: https://opengameart.org/art/audio/sound-effects"
echo "   Search for: arrow, bow, fire, explosion, hit, medieval"
echo ""
echo "2. Kenney.nl: https://kenney.nl/assets"
echo "   Download: Audio Pack SFX"
echo ""
echo "3. Mixkit: https://mixkit.co/free-sound-effects/"
echo "   Search: battle, arrow, fire, hit"
echo ""
echo "After downloading, rename files to match:"
echo "  - tower_arrow_fire.wav"
echo "  - hit_arrow.wav"
echo "  - enemy_death.wav"
echo "  etc."
echo ""
echo "Place all files in: assets/sounds/"
"""

with open("download_sounds_helper.sh", "w") as f:
    f.write(helper_script)

os.chmod("download_sounds_helper.sh", 0o755)

print("✓ Created download_sounds_helper.sh")
print("\n" + "="*60)
print("RECOMMENDATION:")
print("="*60)
print("Due to limitations of automated downloads from free sound sites,")
print("I recommend using a service that provides direct download links.")
print("\nAlternatively, I can create a script that uses:")
print("  - Text-to-speech for simple sounds")
print("  - Sound generation libraries")
print("  - Or guide you through manual download process")
print("="*60)

