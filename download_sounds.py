#!/usr/bin/env python3
"""
Download free sound effects from various public sources.
This script attempts to download appropriate sounds for the game.
"""

import os
import sys
import requests
import time
from urllib.parse import urlparse

# Create sounds directory
os.makedirs("assets/sounds", exist_ok=True)

# Sound file mappings - we'll try to find appropriate sounds
sound_mappings = {
    # Tower sounds - try to find bow/arrow, cannon, fire sounds
    "tower_arrow_fire.wav": [
        "https://opengameart.org/sites/default/files/audio_preview/arrow_0.ogg",
        # We'll need to convert or find WAV versions
    ],
}

def download_file(url, filename, max_retries=3):
    """Download a file from URL with retries."""
    for attempt in range(max_retries):
        try:
            print(f"Attempting to download {filename} from {url}...")
            response = requests.get(url, timeout=10, allow_redirects=True)
            if response.status_code == 200:
                filepath = os.path.join("assets/sounds", filename)
                with open(filepath, 'wb') as f:
                    f.write(response.content)
                print(f"✓ Downloaded: {filename}")
                return True
            else:
                print(f"✗ Failed: HTTP {response.status_code}")
        except Exception as e:
            print(f"✗ Error: {e}")
            if attempt < max_retries - 1:
                time.sleep(2)
    return False

# Since direct downloads are limited, let's try a different approach:
# Use a sound generation library or find public domain sounds
print("Searching for free sound sources...")
print("\nNote: Many free sound sites require manual download.")
print("Attempting to find direct download links...")

# Try to download from known free sound repositories
# Note: Most require manual interaction, so we'll provide instructions

print("\n" + "="*60)
print("AUTOMATED DOWNLOAD LIMITATIONS:")
print("="*60)
print("Most free sound sites (freesound.org, opengameart.org, etc.)")
print("require manual downloads through web interfaces.")
print("\nRECOMMENDED APPROACH:")
print("1. Visit: https://opengameart.org/art/audio/sound-effects")
print("2. Search for: arrow, fire, hit, explosion, medieval")
print("3. Download WAV files and rename them to match our list")
print("4. Or use the script below to help organize downloads")
print("="*60)

