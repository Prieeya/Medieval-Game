# Background Music Directory

This directory contains background music files for the Medieval Siege Defense game.

## Required Music Files

The game expects the following music files:

### Main Background Music
- `background_music.ogg` - Main background music that loops during gameplay

## Supported Formats

- **OGG Vorbis** (`.ogg`) - Recommended, best compression
- **MP3** (`.mp3`) - Also supported
- **WAV** (`.wav`) - Supported but large file size

## Music Recommendations

For intense medieval/siege defense game music, consider:

### Free Music Sources:
1. **OpenGameArt.org** (https://opengameart.org)
   - Search for "medieval", "battle", "epic", "orchestral"
   - Many free tracks with CC0 or CC-BY licenses

2. **Incompetech** (https://incompetech.com)
   - Kevin MacLeod's royalty-free music
   - Search for "epic", "battle", "medieval"
   - Requires attribution

3. **Freesound.org** (https://freesound.org)
   - Some music tracks available
   - Check license requirements

4. **YouTube Audio Library** (https://www.youtube.com/audiolibrary)
   - Free music for games
   - No attribution required for most tracks

### Recommended Styles:
- **Epic orchestral** - Builds tension
- **Medieval battle music** - Fits the theme
- **Intense action music** - Keeps players engaged
- **Looping tracks** - Seamless background music

### File Naming:
- Use lowercase with underscores: `background_music.ogg`
- Keep file sizes reasonable (2-5 MB for OGG)
- Ensure tracks loop seamlessly

## Music Volume

Music volume can be adjusted in `src/Audio/Music.hs`:
- Default: 60/128 (~47%)
- Range: 0-128
- Change `musicVolume` in `initialMusicState`

## How It Works

1. Music starts automatically when the game launches
2. Music loops continuously during gameplay
3. Music pauses when game is paused (Space key)
4. Music resumes when game is unpaused
5. Music stops when game exits

## Adding More Music Tracks

To add different music for different game states:

1. Add music files to `assets/music/`
2. Modify `Main.hs` to play different tracks based on game state:
   - Build phase music
   - Wave combat music
   - Boss battle music
   - Victory/defeat music

## Troubleshooting

- **No music playing**: Check that SDL2 and SDL2-mixer are installed
- **Music file not found**: Ensure file is in `assets/music/` directory
- **Music too loud/quiet**: Adjust volume in `Music.hs` or use system volume

