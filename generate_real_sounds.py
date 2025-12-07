#!/usr/bin/env python3
"""
Generate real sound effects programmatically.
Creates actual audio tones and effects (not silent files).
"""

import struct
import math
import os

def generate_tone(frequency, duration, sample_rate=22050, volume=0.3):
    """Generate a sine wave tone."""
    num_samples = int(sample_rate * duration)
    samples = []
    for i in range(num_samples):
        t = float(i) / sample_rate
        wave = math.sin(2.0 * math.pi * frequency * t)
        samples.append(int(wave * 32767 * volume))
    return samples

def generate_noise(duration, sample_rate=22050, volume=0.2):
    """Generate white noise."""
    import random
    num_samples = int(sample_rate * duration)
    return [random.randint(-32767, 32767) * volume for _ in range(num_samples)]

def generate_impact_sound(duration=0.1):
    """Generate an impact/hit sound (sharp attack with decay)."""
    sample_rate = 22050
    num_samples = int(sample_rate * duration)
    samples = []
    for i in range(num_samples):
        t = float(i) / sample_rate
        # Sharp attack with exponential decay
        attack = min(1.0, t * 100)  # Quick attack
        decay = math.exp(-t * 15)   # Exponential decay
        # Mix of frequencies for impact
        freq1 = 200 + t * 100
        freq2 = 400 - t * 200
        wave1 = math.sin(2.0 * math.pi * freq1 * t) * 0.5
        wave2 = math.sin(2.0 * math.pi * freq2 * t) * 0.3
        value = (wave1 + wave2) * attack * decay * 32767 * 0.4
        samples.append(int(value))
    return samples

def generate_fire_sound(duration=0.3):
    """Generate a fire/explosion sound."""
    sample_rate = 22050
    num_samples = int(sample_rate * duration)
    samples = []
    import random
    for i in range(num_samples):
        t = float(i) / sample_rate
        # Low rumble with crackling
        rumble = math.sin(2.0 * math.pi * 80 * t) * 0.3
        crackle = random.uniform(-0.2, 0.2) if random.random() < 0.3 else 0
        # Decay over time
        envelope = math.exp(-t * 3)
        value = (rumble + crackle) * envelope * 32767 * 0.5
        samples.append(int(value))
    return samples

def generate_arrow_shot(duration=0.15):
    """Generate arrow shooting sound (whoosh)."""
    sample_rate = 22050
    num_samples = int(sample_rate * duration)
    samples = []
    for i in range(num_samples):
        t = float(i) / sample_rate
        # Rising frequency whoosh
        freq = 300 + t * 400
        wave = math.sin(2.0 * math.pi * freq * t)
        # Quick attack, medium decay
        envelope = math.exp(-t * 8) * (1 - math.exp(-t * 50))
        value = wave * envelope * 32767 * 0.3
        samples.append(int(value))
    return samples

def generate_ui_click(duration=0.05):
    """Generate UI click sound (short beep)."""
    return generate_tone(800, duration, volume=0.2)

def generate_error_sound(duration=0.2):
    """Generate error sound (low buzz)."""
    sample_rate = 22050
    num_samples = int(sample_rate * duration)
    samples = []
    for i in range(num_samples):
        t = float(i) / sample_rate
        # Low frequency buzz
        wave = math.sin(2.0 * math.pi * 150 * t)
        envelope = math.exp(-t * 5)
        value = wave * envelope * 32767 * 0.3
        samples.append(int(value))
    return samples

def create_wav_file(filename, samples, sample_rate=22050):
    """Create a WAV file from samples."""
    num_channels = 1
    bits_per_sample = 16
    data_size = len(samples) * num_channels * (bits_per_sample // 8)
    
    with open(filename, 'wb') as f:
        # RIFF header
        f.write(b'RIFF')
        f.write(struct.pack('<I', 36 + data_size))
        f.write(b'WAVE')
        
        # fmt chunk
        f.write(b'fmt ')
        f.write(struct.pack('<I', 16))
        f.write(struct.pack('<H', 1))  # PCM
        f.write(struct.pack('<H', num_channels))
        f.write(struct.pack('<I', sample_rate))
        f.write(struct.pack('<I', sample_rate * num_channels * (bits_per_sample // 8)))
        f.write(struct.pack('<H', num_channels * (bits_per_sample // 8)))
        f.write(struct.pack('<H', bits_per_sample))
        
        # data chunk
        f.write(b'data')
        f.write(struct.pack('<I', data_size))
        for sample in samples:
            f.write(struct.pack('<h', int(sample)))

# Sound generation mappings
sound_generators = {
    # Tower sounds
    "tower_arrow_fire.wav": lambda: generate_arrow_shot(0.15),
    "tower_catapult_fire.wav": lambda: generate_fire_sound(0.4),
    "tower_crossbow_fire.wav": lambda: generate_arrow_shot(0.12),
    "tower_fire_fire.wav": lambda: generate_fire_sound(0.3),
    "tower_tesla_fire.wav": lambda: generate_tone(600, 0.1) + generate_tone(1200, 0.1),
    "tower_ballista_fire.wav": lambda: generate_arrow_shot(0.2),
    "tower_poison_fire.wav": lambda: generate_tone(400, 0.15),
    "tower_bombard_fire.wav": lambda: generate_fire_sound(0.5),
    "build_tower.wav": lambda: generate_tone(500, 0.2),
    "sell_tower.wav": lambda: generate_tone(300, 0.15),
    "upgrade_tower.wav": lambda: generate_tone(600, 0.25),
    "structure_destroyed.wav": lambda: generate_fire_sound(0.6),
    
    # Combat & Impact
    "hit_arrow.wav": lambda: generate_impact_sound(0.1),
    "hit_heavy.wav": lambda: generate_impact_sound(0.15),
    "hit_fire.wav": lambda: generate_fire_sound(0.2),
    "hit_ice.wav": lambda: generate_tone(800, 0.1) + generate_tone(1600, 0.1),
    "hit_lightning.wav": lambda: generate_tone(1000, 0.05) + generate_tone(2000, 0.05),
    "hit_rock.wav": lambda: generate_impact_sound(0.2),
    "enemy_hit.wav": lambda: generate_impact_sound(0.1),
    "enemy_death.wav": lambda: generate_tone(200, 0.3),
    "enemy_attack.wav": lambda: generate_impact_sound(0.15),
    "trap_trigger.wav": lambda: generate_impact_sound(0.12),
    
    # Fort & Structure
    "gate_hit.wav": lambda: generate_impact_sound(0.2),
    "gate_destroyed.wav": lambda: generate_fire_sound(0.8),
    "repair.wav": lambda: generate_tone(400, 0.3),
    "wall_hit.wav": lambda: generate_impact_sound(0.15),
    "castle_hit.wav": lambda: generate_impact_sound(0.25),
    
    # UI & Gameplay
    "wave_start.wav": lambda: generate_tone(300, 0.5),
    "wave_complete.wav": lambda: generate_tone(500, 0.4) + generate_tone(600, 0.2),
    "level_complete.wav": lambda: [tone(400, 0.2) + tone(500, 0.2) + tone(600, 0.2) for tone in [generate_tone]],
    "game_over.wav": lambda: generate_tone(200, 0.8),
    "victory.wav": lambda: generate_tone(523, 0.2) + generate_tone(659, 0.2) + generate_tone(784, 0.3),
    "ui_hover.wav": lambda: generate_tone(600, 0.05),
    "ui_click.wav": lambda: generate_ui_click(0.05),
    "ui_error.wav": lambda: generate_error_sound(0.2),
    "upgrade_unlock.wav": lambda: generate_tone(600, 0.3),
}

# Fix the level_complete generator
def generate_level_complete():
    return generate_tone(400, 0.2) + generate_tone(500, 0.2) + generate_tone(600, 0.2)

def generate_victory():
    return generate_tone(523, 0.2) + generate_tone(659, 0.2) + generate_tone(784, 0.3)

sound_generators["level_complete.wav"] = generate_level_complete
sound_generators["victory.wav"] = generate_victory

# Create sounds directory
os.makedirs("assets/sounds", exist_ok=True)

print("Generating real sound effects (with actual audio)...")
print("="*60)

for filename, generator in sound_generators.items():
    try:
        filepath = os.path.join("assets/sounds", filename)
        samples = generator()
        create_wav_file(filepath, samples)
        print(f"✓ Generated: {filename} ({len(samples)} samples)")
    except Exception as e:
        print(f"✗ Failed to generate {filename}: {e}")

print("="*60)
print(f"✓ Generated {len(sound_generators)} sound files with real audio!")
print("These files contain actual sound waves (tones, impacts, etc.)")
print("They will play in the game (not silent).")

