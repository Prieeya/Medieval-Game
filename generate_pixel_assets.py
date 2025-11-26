#!/usr/bin/env python3
"""
Complete Pixel Art Asset Generator for Medieval Siege Simulator
Generates all 32x32 pixel art sprites as PNG files
Top-down orthographic view, SNES/GBA style
"""

import os
from PIL import Image, ImageDraw

# ============================================================================
# Color Palette (SNES/GBA Style)
# ============================================================================

COLORS = {
    # Stone & Castle
    'stone_light': (180, 180, 200), 'stone': (140, 140, 160), 'stone_dark': (100, 100, 120),
    'iron': (200, 200, 220), 'iron_dark': (120, 120, 140),
    
    # Wood
    'wood_light': (180, 140, 100), 'wood': (140, 100, 60), 'wood_dark': (100, 70, 40),
    
    # Nature
    'grass_light': (120, 180, 100), 'grass': (80, 140, 60), 'grass_dark': (60, 100, 40),
    'dirt_light': (160, 120, 80), 'dirt': (120, 80, 50), 'dirt_dark': (80, 60, 40),
    
    # Fire
    'fire_yellow': (255, 255, 100), 'fire_orange': (255, 180, 60), 'fire_red': (220, 60, 40),
    
    # Magic
    'magic_blue': (100, 150, 255), 'magic_light_blue': (150, 200, 255),
    'magic_purple': (180, 100, 220), 'magic_light_purple': (220, 150, 255),
    
    # Ice
    'ice_blue': (150, 200, 255), 'ice_light': (200, 230, 255), 'ice_white': (255, 255, 255),
    
    # Poison
    'poison_green': (60, 200, 100), 'poison_dark_green': (40, 140, 60),
    
    # Characters
    'skin_light': (255, 220, 180), 'skin': (220, 180, 140), 'skin_dark': (180, 140, 100),
    'leather': (100, 80, 60), 'armor': (180, 180, 200), 'armor_dark': (120, 120, 140),
    'orc_skin': (100, 140, 100), 'orc_skin_dark': (60, 100, 60),
    
    # Misc
    'black': (20, 20, 20), 'white': (255, 255, 255), 'red': (220, 60, 60), 'blue': (60, 100, 220),
    'gold': (255, 220, 100), 'shadow': (40, 40, 60), 'green': (60, 180, 100),
    'bone': (240, 240, 220), 'bone_dark': (200, 200, 180),
}

def c(name): return COLORS.get(name, COLORS['black'])

# ============================================================================
# Helper Functions
# ============================================================================

def create_sprite():
    """Create a new transparent 32x32 sprite"""
    img = Image.new('RGBA', (32, 32), (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)
    return img, draw

def set_pixel(draw, x, y, color):
    """Set a pixel at position"""
    if 0 <= x < 32 and 0 <= y < 32:
        draw.rectangle([x, y, x, y], fill=color)

def draw_pixels(draw, pixels, offset_x=16, offset_y=16):
    """Draw pixels from list of (x, y, color_name) tuples"""
    for x, y, col in pixels:
        set_pixel(draw, offset_x + x, offset_y + y, c(col))

def save_sprite(img, path):
    """Save sprite ensuring directory exists"""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    img.save(path, 'PNG')
    print(f"✓ {path}")

# ============================================================================
# Environment Tiles
# ============================================================================

def generate_castle_wall_horizontal():
    img, draw = create_sprite()
    # Stone wall horizontal - top-down view
    for y in range(10, 22):
        for x in range(2, 30):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Top edge shadow
    for x in range(2, 30):
        set_pixel(draw, x, 10, c('stone_dark'))
    # Mortar lines
    for x in range(2, 30):
        if x % 4 == 0:
            set_pixel(draw, x, 14, c('stone_dark'))
            set_pixel(draw, x, 18, c('stone_dark'))
    return img

def generate_castle_wall_vertical():
    img, draw = create_sprite()
    # Stone wall vertical
    for x in range(10, 22):
        for y in range(2, 30):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Left edge shadow
    for y in range(2, 30):
        set_pixel(draw, 10, y, c('stone_dark'))
    # Mortar lines
    for y in range(2, 30):
        if y % 4 == 0:
            set_pixel(draw, 14, y, c('stone_dark'))
            set_pixel(draw, 18, y, c('stone_dark'))
    return img

def generate_castle_wall_corner_NE():
    img, draw = create_sprite()
    # Corner piece - Northeast
    for y in range(10, 22):
        for x in range(10, 22):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Edges
    for x in range(10, 22):
        set_pixel(draw, x, 10, c('stone_dark'))
    for y in range(10, 22):
        set_pixel(draw, 21, y, c('stone_dark'))
    return img

def generate_castle_wall_corner_NW():
    img, draw = create_sprite()
    # Corner piece - Northwest
    for y in range(10, 22):
        for x in range(10, 22):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Edges
    for x in range(10, 22):
        set_pixel(draw, x, 10, c('stone_dark'))
    for y in range(10, 22):
        set_pixel(draw, 10, y, c('stone_dark'))
    return img

def generate_castle_wall_corner_SE():
    img, draw = create_sprite()
    # Corner piece - Southeast
    for y in range(10, 22):
        for x in range(10, 22):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Edges
    for x in range(10, 22):
        set_pixel(draw, x, 21, c('stone_dark'))
    for y in range(10, 22):
        set_pixel(draw, 21, y, c('stone_dark'))
    return img

def generate_castle_wall_corner_SW():
    img, draw = create_sprite()
    # Corner piece - Southwest
    for y in range(10, 22):
        for x in range(10, 22):
            if (x + y) % 3 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Edges
    for x in range(10, 22):
        set_pixel(draw, x, 21, c('stone_dark'))
    for y in range(10, 22):
        set_pixel(draw, 10, y, c('stone_dark'))
    return img

def generate_castle_gate_closed():
    img, draw = create_sprite()
    # Wooden gate - closed
    for y in range(8, 24):
        for x in range(8, 24):
            if (x + y) % 2 == 0:
                set_pixel(draw, x, y, c('wood'))
            else:
                set_pixel(draw, x, y, c('wood_dark'))
    # Iron bands
    for x in range(8, 24):
        set_pixel(draw, x, 12, c('iron_dark'))
        set_pixel(draw, x, 16, c('iron_dark'))
        set_pixel(draw, x, 20, c('iron_dark'))
    # Iron hinges
    set_pixel(draw, 8, 14, c('iron'))
    set_pixel(draw, 8, 18, c('iron'))
    set_pixel(draw, 23, 14, c('iron'))
    set_pixel(draw, 23, 18, c('iron'))
    return img

def generate_castle_gate_open_top():
    img, draw = create_sprite()
    # Gate open - top half
    for y in range(8, 16):
        for x in range(8, 24):
            if (x + y) % 2 == 0:
                set_pixel(draw, x, y, c('wood'))
            else:
                set_pixel(draw, x, y, c('wood_dark'))
    # Iron band
    for x in range(8, 24):
        set_pixel(draw, x, 12, c('iron_dark'))
    return img

def generate_castle_gate_open_bottom():
    img, draw = create_sprite()
    # Gate open - bottom half
    for y in range(16, 24):
        for x in range(8, 24):
            if (x + y) % 2 == 0:
                set_pixel(draw, x, y, c('wood'))
            else:
                set_pixel(draw, x, y, c('wood_dark'))
    # Iron bands
    for x in range(8, 24):
        set_pixel(draw, x, 16, c('iron_dark'))
        set_pixel(draw, x, 20, c('iron_dark'))
    return img

def generate_castle_floor_stone():
    img, draw = create_sprite()
    # Stone floor tiles
    for y in range(32):
        for x in range(32):
            if (x // 4 + y // 4) % 2 == 0:
                set_pixel(draw, x, y, c('stone_light'))
            else:
                set_pixel(draw, x, y, c('stone'))
    # Grid lines
    for x in range(0, 32, 4):
        for y in range(32):
            set_pixel(draw, x, y, c('stone_dark'))
    for y in range(0, 32, 4):
        for x in range(32):
            set_pixel(draw, x, y, c('stone_dark'))
    return img

def generate_grass_tile(variant='A'):
    img, draw = create_sprite()
    # Base grass
    for y in range(32):
        for x in range(32):
            if (x + y) % 2 == 0:
                set_pixel(draw, x, y, c('grass'))
            else:
                set_pixel(draw, x, y, c('grass_light'))
    # Add small flowers for variation
    if variant == 'A':
        set_pixel(draw, 8, 8, c('fire_yellow'))
        set_pixel(draw, 24, 20, c('fire_yellow'))
    elif variant == 'B':
        set_pixel(draw, 12, 16, c('magic_purple'))
        set_pixel(draw, 20, 8, c('magic_purple'))
    elif variant == 'C':
        set_pixel(draw, 16, 12, c('fire_yellow'))
        set_pixel(draw, 6, 24, c('magic_purple'))
    return img

def generate_path_tile(variant='A'):
    img, draw = create_sprite()
    # Dirt path
    for y in range(32):
        for x in range(32):
            if (x + y) % 2 == 0:
                set_pixel(draw, x, y, c('dirt'))
            else:
                set_pixel(draw, x, y, c('dirt_light'))
    # Add pebbles
    if variant == 'A':
        set_pixel(draw, 10, 10, c('stone'))
        set_pixel(draw, 22, 18, c('stone'))
    elif variant == 'B':
        set_pixel(draw, 14, 8, c('stone'))
        set_pixel(draw, 18, 24, c('stone'))
    elif variant == 'C':
        set_pixel(draw, 8, 20, c('stone'))
        set_pixel(draw, 24, 12, c('stone'))
    return img

# ============================================================================
# Towers
# ============================================================================

def generate_arrow_tower_idle(frame=0):
    img, draw = create_sprite()
    # Wooden tower base
    pixels = [
        # Base
        (-5, 6, 'wood'), (-4, 6, 'wood_dark'), (-3, 6, 'wood'), (-2, 6, 'wood_dark'),
        (-1, 6, 'wood'), (0, 6, 'wood_dark'), (1, 6, 'wood'), (2, 6, 'wood_dark'),
        (3, 6, 'wood'), (4, 6, 'wood_dark'), (5, 6, 'wood'),
        (-4, 7, 'wood_dark'), (-3, 7, 'wood'), (-2, 7, 'wood_dark'), (-1, 7, 'wood'),
        (0, 7, 'wood_dark'), (1, 7, 'wood'), (2, 7, 'wood_dark'), (3, 7, 'wood'), (4, 7, 'wood_dark'),
        (-3, 8, 'wood'), (-2, 8, 'wood_dark'), (-1, 8, 'wood'), (0, 8, 'wood_dark'),
        (1, 8, 'wood'), (2, 8, 'wood_dark'), (3, 8, 'wood'),
        # Tower body
        (-2, 4, 'wood'), (-1, 4, 'wood_dark'), (0, 4, 'wood'), (1, 4, 'wood_dark'), (2, 4, 'wood'),
        (-2, 5, 'wood_dark'), (-1, 5, 'wood'), (0, 5, 'wood_dark'), (1, 5, 'wood'), (2, 5, 'wood_dark'),
        # Platform
        (-3, 3, 'wood'), (-2, 3, 'wood_dark'), (-1, 3, 'wood'), (0, 3, 'wood_dark'),
        (1, 3, 'wood'), (2, 3, 'wood_dark'), (3, 3, 'wood'),
        # Archer on top
        (0, 1, 'skin'), (-1, 0, 'leather'), (0, 0, 'skin'), (1, 0, 'leather'),
        (0, -1, 'leather'), (0, -2, 'leather'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_arrow_tower_attack(frame=0):
    img, draw = create_sprite()
    # Same base as idle
    pixels = [
        # Base
        (-5, 6, 'wood'), (-4, 6, 'wood_dark'), (-3, 6, 'wood'), (-2, 6, 'wood_dark'),
        (-1, 6, 'wood'), (0, 6, 'wood_dark'), (1, 6, 'wood'), (2, 6, 'wood_dark'),
        (3, 6, 'wood'), (4, 6, 'wood_dark'), (5, 6, 'wood'),
        (-4, 7, 'wood_dark'), (-3, 7, 'wood'), (-2, 7, 'wood_dark'), (-1, 7, 'wood'),
        (0, 7, 'wood_dark'), (1, 7, 'wood'), (2, 7, 'wood_dark'), (3, 7, 'wood'), (4, 7, 'wood_dark'),
        (-3, 8, 'wood'), (-2, 8, 'wood_dark'), (-1, 8, 'wood'), (0, 8, 'wood_dark'),
        (1, 8, 'wood'), (2, 8, 'wood_dark'), (3, 8, 'wood'),
        # Tower body
        (-2, 4, 'wood'), (-1, 4, 'wood_dark'), (0, 4, 'wood'), (1, 4, 'wood_dark'), (2, 4, 'wood'),
        (-2, 5, 'wood_dark'), (-1, 5, 'wood'), (0, 5, 'wood_dark'), (1, 5, 'wood'), (2, 5, 'wood_dark'),
        # Platform
        (-3, 3, 'wood'), (-2, 3, 'wood_dark'), (-1, 3, 'wood'), (0, 3, 'wood_dark'),
        (1, 3, 'wood'), (2, 3, 'wood_dark'), (3, 3, 'wood'),
        # Archer shooting (bow extended)
        (0, 1, 'skin'), (-1, 0, 'leather'), (0, 0, 'skin'), (1, 0, 'leather'),
        (0, -1, 'leather'), (2, 0, 'wood'), (3, 0, 'wood'),  # Bow
        (4, 0, 'wood'),  # Arrow
    ]
    draw_pixels(draw, pixels)
    return img

def generate_arrow_tower_death(frame=0):
    img, draw = create_sprite()
    # Collapsed tower
    pixels = [
        (-3, 6, 'wood_dark'), (-2, 6, 'wood'), (-1, 6, 'wood_dark'), (0, 6, 'wood'),
        (1, 6, 'wood_dark'), (2, 6, 'wood'), (3, 6, 'wood_dark'),
        (-2, 7, 'wood'), (-1, 7, 'wood_dark'), (0, 7, 'wood'), (1, 7, 'wood_dark'), (2, 7, 'wood'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_catapult_tower_idle(frame=0):
    img, draw = create_sprite()
    # Stone base with wooden catapult
    pixels = [
        # Stone base
        (-5, 7, 'stone'), (-4, 7, 'stone_dark'), (-3, 7, 'stone'), (-2, 7, 'stone_dark'),
        (-1, 7, 'stone'), (0, 7, 'stone_dark'), (1, 7, 'stone'), (2, 7, 'stone_dark'),
        (3, 7, 'stone'), (4, 7, 'stone_dark'), (5, 7, 'stone'),
        (-4, 8, 'stone_dark'), (-3, 8, 'stone'), (-2, 8, 'stone_dark'), (-1, 8, 'stone'),
        (0, 8, 'stone_dark'), (1, 8, 'stone'), (2, 8, 'stone_dark'), (3, 8, 'stone'), (4, 8, 'stone_dark'),
        # Catapult frame
        (-3, 4, 'wood'), (-2, 4, 'wood_dark'), (-1, 4, 'wood'), (0, 4, 'wood_dark'),
        (1, 4, 'wood'), (2, 4, 'wood_dark'), (3, 4, 'wood'),
        # Catapult arm
        (-2, 2, 'wood'), (-1, 2, 'wood_dark'), (0, 2, 'wood'), (1, 2, 'wood_dark'), (2, 2, 'wood'),
        (-1, 1, 'wood'), (0, 1, 'wood_dark'), (1, 1, 'wood'),
        (0, 0, 'wood'),  # Arm tip
    ]
    draw_pixels(draw, pixels)
    return img

def generate_catapult_tower_attack(frame=0):
    img, draw = create_sprite()
    # Same as idle but arm rotated
    pixels = [
        # Stone base
        (-5, 7, 'stone'), (-4, 7, 'stone_dark'), (-3, 7, 'stone'), (-2, 7, 'stone_dark'),
        (-1, 7, 'stone'), (0, 7, 'stone_dark'), (1, 7, 'stone'), (2, 7, 'stone_dark'),
        (3, 7, 'stone'), (4, 7, 'stone_dark'), (5, 7, 'stone'),
        (-4, 8, 'stone_dark'), (-3, 8, 'stone'), (-2, 8, 'stone_dark'), (-1, 8, 'stone'),
        (0, 8, 'stone_dark'), (1, 8, 'stone'), (2, 8, 'stone_dark'), (3, 8, 'stone'), (4, 8, 'stone_dark'),
        # Catapult frame
        (-3, 4, 'wood'), (-2, 4, 'wood_dark'), (-1, 4, 'wood'), (0, 4, 'wood_dark'),
        (1, 4, 'wood'), (2, 4, 'wood_dark'), (3, 4, 'wood'),
        # Catapult arm (rotated back)
        (-1, 3, 'wood'), (0, 3, 'wood_dark'), (1, 3, 'wood'),
        (0, 2, 'wood'), (1, 2, 'wood_dark'), (2, 2, 'wood'),
        (1, 1, 'wood'), (2, 1, 'wood_dark'), (3, 1, 'wood'),
        (2, 0, 'wood'), (3, 0, 'wood'),  # Arm tip
    ]
    draw_pixels(draw, pixels)
    return img

def generate_crossbow_tower_idle(frame=0):
    img, draw = create_sprite()
    # Reinforced wooden turret
    pixels = [
        # Base
        (-4, 6, 'wood'), (-3, 6, 'wood_dark'), (-2, 6, 'wood'), (-1, 6, 'wood_dark'),
        (0, 6, 'wood'), (1, 6, 'wood_dark'), (2, 6, 'wood'), (3, 6, 'wood_dark'), (4, 6, 'wood'),
        (-3, 7, 'wood_dark'), (-2, 7, 'wood'), (-1, 7, 'wood_dark'), (0, 7, 'wood'),
        (1, 7, 'wood_dark'), (2, 7, 'wood'), (3, 7, 'wood_dark'),
        # Tower body
        (-2, 4, 'wood'), (-1, 4, 'iron'), (0, 4, 'wood'), (1, 4, 'iron'), (2, 4, 'wood'),
        (-2, 5, 'iron'), (-1, 5, 'wood'), (0, 5, 'iron'), (1, 5, 'wood'), (2, 5, 'iron'),
        # Crossbow
        (-3, 2, 'wood'), (-2, 2, 'iron'), (-1, 2, 'wood'), (0, 2, 'iron'), (1, 2, 'wood'),
        (2, 2, 'iron'), (3, 2, 'wood'),
        (0, 1, 'iron'),  # Crossbow center
    ]
    draw_pixels(draw, pixels)
    return img

def generate_fire_tower_idle(frame=0):
    img, draw = create_sprite()
    # Iron brazier tower
    pixels = [
        # Stone base
        (-4, 7, 'stone'), (-3, 7, 'stone_dark'), (-2, 7, 'stone'), (-1, 7, 'stone_dark'),
        (0, 7, 'stone'), (1, 7, 'stone_dark'), (2, 7, 'stone'), (3, 7, 'stone_dark'), (4, 7, 'stone'),
        # Iron tower
        (-2, 4, 'iron'), (-1, 4, 'iron_dark'), (0, 4, 'iron'), (1, 4, 'iron_dark'), (2, 4, 'iron'),
        (-2, 5, 'iron_dark'), (-1, 5, 'iron'), (0, 5, 'iron_dark'), (1, 5, 'iron'), (2, 5, 'iron_dark'),
        # Brazier
        (-1, 2, 'iron_dark'), (0, 2, 'fire_red'), (1, 2, 'iron_dark'),
        (0, 1, 'fire_orange'), (0, 0, 'fire_yellow'),  # Flame
    ]
    draw_pixels(draw, pixels)
    return img

def generate_fire_tower_attack(frame=0):
    img, draw = create_sprite()
    # Larger flame
    pixels = [
        # Stone base
        (-4, 7, 'stone'), (-3, 7, 'stone_dark'), (-2, 7, 'stone'), (-1, 7, 'stone_dark'),
        (0, 7, 'stone'), (1, 7, 'stone_dark'), (2, 7, 'stone'), (3, 7, 'stone_dark'), (4, 7, 'stone'),
        # Iron tower
        (-2, 4, 'iron'), (-1, 4, 'iron_dark'), (0, 4, 'iron'), (1, 4, 'iron_dark'), (2, 4, 'iron'),
        (-2, 5, 'iron_dark'), (-1, 5, 'iron'), (0, 5, 'iron_dark'), (1, 5, 'iron'), (2, 5, 'iron_dark'),
        # Larger brazier flame
        (-1, 2, 'iron_dark'), (0, 2, 'fire_red'), (1, 2, 'iron_dark'),
        (-1, 1, 'fire_orange'), (0, 1, 'fire_yellow'), (1, 1, 'fire_orange'),
        (-1, 0, 'fire_yellow'), (0, 0, 'fire_yellow'), (1, 0, 'fire_yellow'),
        (0, -1, 'fire_yellow'),  # Flame tip
    ]
    draw_pixels(draw, pixels)
    return img

def generate_tesla_tower_idle(frame=0):
    img, draw = create_sprite()
    # Iron spire with lightning orb
    pixels = [
        # Stone base
        (-3, 7, 'stone'), (-2, 7, 'stone_dark'), (-1, 7, 'stone'), (0, 7, 'stone_dark'),
        (1, 7, 'stone'), (2, 7, 'stone_dark'), (3, 7, 'stone'),
        # Iron spire
        (-1, 4, 'iron'), (0, 4, 'iron_dark'), (1, 4, 'iron'),
        (-1, 5, 'iron_dark'), (0, 5, 'iron'), (1, 5, 'iron_dark'),
        # Lightning orb
        (-1, 2, 'magic_blue'), (0, 2, 'magic_light_blue'), (1, 2, 'magic_blue'),
        (0, 1, 'white'),  # Core
        # Small sparks
        (-2, 1, 'magic_light_blue'), (2, 1, 'magic_light_blue'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_tesla_tower_attack(frame=0):
    img, draw = create_sprite()
    # Lightning arcs
    pixels = [
        # Stone base
        (-3, 7, 'stone'), (-2, 7, 'stone_dark'), (-1, 7, 'stone'), (0, 7, 'stone_dark'),
        (1, 7, 'stone'), (2, 7, 'stone_dark'), (3, 7, 'stone'),
        # Iron spire
        (-1, 4, 'iron'), (0, 4, 'iron_dark'), (1, 4, 'iron'),
        (-1, 5, 'iron_dark'), (0, 5, 'iron'), (1, 5, 'iron_dark'),
        # Lightning orb (brighter)
        (-1, 2, 'magic_blue'), (0, 2, 'white'), (1, 2, 'magic_blue'),
        (0, 1, 'white'),
        # Lightning arcs
        (-3, 0, 'magic_light_blue'), (-2, 1, 'magic_light_blue'), (-1, 0, 'magic_light_blue'),
        (1, 0, 'magic_light_blue'), (2, 1, 'magic_light_blue'), (3, 0, 'magic_light_blue'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_ballista_tower_idle(frame=0):
    img, draw = create_sprite()
    # Large wooden ballista
    pixels = [
        # Base
        (-4, 7, 'wood'), (-3, 7, 'wood_dark'), (-2, 7, 'wood'), (-1, 7, 'wood_dark'),
        (0, 7, 'wood'), (1, 7, 'wood_dark'), (2, 7, 'wood'), (3, 7, 'wood_dark'), (4, 7, 'wood'),
        # Ballista frame
        (-3, 4, 'wood'), (-2, 4, 'iron'), (-1, 4, 'wood'), (0, 4, 'iron'), (1, 4, 'wood'),
        (2, 4, 'iron'), (3, 4, 'wood'),
        # Ballista arms
        (-3, 3, 'wood'), (-2, 3, 'iron'), (2, 3, 'iron'), (3, 3, 'wood'),
        # Bolt
        (-2, 2, 'iron'), (-1, 2, 'iron_dark'), (0, 2, 'iron'), (1, 2, 'iron_dark'), (2, 2, 'iron'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_poison_tower_idle(frame=0):
    img, draw = create_sprite()
    # Green bubbling cauldron
    pixels = [
        # Wooden stand
        (-3, 7, 'wood'), (-2, 7, 'wood_dark'), (-1, 7, 'wood'), (0, 7, 'wood_dark'),
        (1, 7, 'wood'), (2, 7, 'wood_dark'), (3, 7, 'wood'),
        # Cauldron
        (-2, 4, 'iron_dark'), (-1, 4, 'iron'), (0, 4, 'iron_dark'), (1, 4, 'iron'), (2, 4, 'iron_dark'),
        (-2, 5, 'iron'), (-1, 5, 'poison_green'), (0, 5, 'iron'), (1, 5, 'poison_green'), (2, 5, 'iron'),
        # Poison bubbles
        (-1, 3, 'poison_green'), (0, 3, 'poison_dark_green'), (1, 3, 'poison_green'),
        (0, 2, 'poison_green'),  # Bubble
    ]
    draw_pixels(draw, pixels)
    return img

def generate_bombard_tower_idle(frame=0):
    img, draw = create_sprite()
    # Iron cannon on stone base
    pixels = [
        # Stone base
        (-4, 7, 'stone'), (-3, 7, 'stone_dark'), (-2, 7, 'stone'), (-1, 7, 'stone_dark'),
        (0, 7, 'stone'), (1, 7, 'stone_dark'), (2, 7, 'stone'), (3, 7, 'stone_dark'), (4, 7, 'stone'),
        # Cannon base
        (-2, 5, 'iron'), (-1, 5, 'iron_dark'), (0, 5, 'iron'), (1, 5, 'iron_dark'), (2, 5, 'iron'),
        # Cannon barrel
        (-2, 3, 'iron_dark'), (-1, 3, 'black'), (0, 3, 'iron_dark'), (1, 3, 'black'), (2, 3, 'iron_dark'),
        (-1, 2, 'black'), (0, 2, 'iron_dark'), (1, 2, 'black'),
        (0, 1, 'black'),  # Cannon mouth
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# Traps
# ============================================================================

def generate_spike_trap_idle(frame=0):
    img, draw = create_sprite()
    # Hidden spikes
    pixels = [
        # Ground
        (-4, 6, 'dirt'), (-3, 6, 'dirt_dark'), (-2, 6, 'dirt'), (-1, 6, 'dirt_dark'),
        (0, 6, 'dirt'), (1, 6, 'dirt_dark'), (2, 6, 'dirt'), (3, 6, 'dirt_dark'), (4, 6, 'dirt'),
        # Hidden spikes (small)
        (-1, 5, 'iron_dark'), (0, 5, 'iron'), (1, 5, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_spike_trap_trigger(frame=0):
    img, draw = create_sprite()
    # Extended spikes
    pixels = [
        # Ground
        (-4, 6, 'dirt'), (-3, 6, 'dirt_dark'), (-2, 6, 'dirt'), (-1, 6, 'dirt_dark'),
        (0, 6, 'dirt'), (1, 6, 'dirt_dark'), (2, 6, 'dirt'), (3, 6, 'dirt_dark'), (4, 6, 'dirt'),
        # Extended spikes
        (-1, 3, 'iron_dark'), (0, 3, 'iron'), (1, 3, 'iron_dark'),
        (-1, 4, 'iron'), (0, 4, 'iron'), (1, 4, 'iron'),
        (-1, 5, 'iron'), (0, 5, 'iron'), (1, 5, 'iron'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_freeze_trap_idle(frame=0):
    img, draw = create_sprite()
    # Frost crystal
    pixels = [
        # Ground
        (-3, 6, 'dirt'), (-2, 6, 'dirt_dark'), (-1, 6, 'dirt'), (0, 6, 'dirt_dark'),
        (1, 6, 'dirt'), (2, 6, 'dirt_dark'), (3, 6, 'dirt'),
        # Frost crystal
        (0, 4, 'ice_blue'), (-1, 3, 'ice_light'), (0, 3, 'ice_white'), (1, 3, 'ice_light'),
        (0, 2, 'ice_blue'), (-1, 1, 'ice_light'), (0, 1, 'ice_white'), (1, 1, 'ice_light'),
        (0, 0, 'ice_blue'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_fire_pit_trap_idle(frame=0):
    img, draw = create_sprite()
    # Glowing crack
    pixels = [
        # Ground
        (-3, 6, 'dirt'), (-2, 6, 'dirt_dark'), (-1, 6, 'dirt'), (0, 6, 'dirt_dark'),
        (1, 6, 'dirt'), (2, 6, 'dirt_dark'), (3, 6, 'dirt'),
        # Fire crack
        (-1, 4, 'fire_red'), (0, 4, 'fire_orange'), (1, 4, 'fire_red'),
        (0, 3, 'fire_yellow'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_magic_snare_trap_idle(frame=0):
    img, draw = create_sprite()
    # Purple rune circle
    pixels = [
        # Ground
        (-3, 6, 'dirt'), (-2, 6, 'dirt_dark'), (-1, 6, 'dirt'), (0, 6, 'dirt_dark'),
        (1, 6, 'dirt'), (2, 6, 'dirt_dark'), (3, 6, 'dirt'),
        # Rune circle
        (-2, 3, 'magic_purple'), (-1, 3, 'magic_light_purple'), (0, 3, 'magic_purple'),
        (1, 3, 'magic_light_purple'), (2, 3, 'magic_purple'),
        (-1, 2, 'magic_purple'), (0, 2, 'magic_light_purple'), (1, 2, 'magic_purple'),
        (0, 1, 'magic_purple'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_explosive_barrel_idle(frame=0):
    img, draw = create_sprite()
    # Wooden barrel
    pixels = [
        # Barrel body
        (-2, 4, 'wood'), (-1, 4, 'wood_dark'), (0, 4, 'wood'), (1, 4, 'wood_dark'), (2, 4, 'wood'),
        (-2, 5, 'wood_dark'), (-1, 5, 'wood'), (0, 5, 'red'), (1, 5, 'wood'), (2, 5, 'wood_dark'),
        (-2, 6, 'wood'), (-1, 6, 'wood_dark'), (0, 6, 'wood'), (1, 6, 'wood_dark'), (2, 6, 'wood'),
        # Metal bands
        (-2, 4, 'iron_dark'), (2, 4, 'iron_dark'),
        (-2, 6, 'iron_dark'), (2, 6, 'iron_dark'),
        # Fuse
        (0, 3, 'fire_red'), (0, 2, 'fire_orange'), (0, 1, 'fire_yellow'),
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# Enemies
# ============================================================================

def generate_grunt_raider_idle(frame=0):
    img, draw = create_sprite()
    # Small humanoid in leather armor
    pixels = [
        # Head
        (0, -3, 'skin'),
        # Body
        (-1, -1, 'leather'), (0, -1, 'skin'), (1, -1, 'leather'),
        (-1, 0, 'leather'), (0, 0, 'skin'), (1, 0, 'leather'),
        # Legs
        (-1, 2, 'leather'), (1, 2, 'leather'),
        # Rusty axe
        (0, 3, 'iron_dark'), (1, 3, 'wood'), (0, 4, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_grunt_raider_move(frame=0):
    img, draw = create_sprite()
    # Running pose
    pixels = [
        # Head
        (0, -3, 'skin'),
        # Body (slightly forward)
        (-1, -1, 'leather'), (0, -1, 'skin'), (1, -1, 'leather'),
        (-1, 0, 'leather'), (0, 0, 'skin'), (1, 0, 'leather'),
        # Running legs
        (-2, 2, 'leather'), (2, 2, 'leather'),
        # Axe held back
        (-1, 3, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_grunt_raider_attack(frame=0):
    img, draw = create_sprite()
    # Swinging axe
    pixels = [
        # Head
        (0, -3, 'skin'),
        # Body
        (-1, -1, 'leather'), (0, -1, 'skin'), (1, -1, 'leather'),
        (-1, 0, 'leather'), (0, 0, 'skin'), (1, 0, 'leather'),
        # Legs
        (-1, 2, 'leather'), (1, 2, 'leather'),
        # Swinging axe
        (-2, 2, 'iron_dark'), (-1, 2, 'wood'), (0, 2, 'iron_dark'),
        (-2, 1, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_brute_crusher_idle(frame=0):
    img, draw = create_sprite()
    # Large orc brute
    pixels = [
        # Head
        (0, -4, 'orc_skin'),
        (-1, -3, 'orc_skin'), (0, -3, 'orc_skin_dark'), (1, -3, 'orc_skin'),
        # Body
        (-2, -1, 'orc_skin'), (-1, -1, 'orc_skin_dark'), (0, -1, 'orc_skin'),
        (1, -1, 'orc_skin_dark'), (2, -1, 'orc_skin'),
        (-2, 0, 'orc_skin_dark'), (-1, 0, 'orc_skin'), (0, 0, 'orc_skin_dark'),
        (1, 0, 'orc_skin'), (2, 0, 'orc_skin_dark'),
        # Legs
        (-1, 2, 'orc_skin'), (1, 2, 'orc_skin'),
        # Club
        (0, 3, 'wood'), (0, 4, 'wood_dark'), (0, 5, 'wood'),
        (-1, 4, 'wood'), (1, 4, 'wood'),  # Club head
    ]
    draw_pixels(draw, pixels)
    return img

def generate_direwolf_idle(frame=0):
    img, draw = create_sprite()
    # Dark wolf
    pixels = [
        # Head
        (0, -3, 'black'),
        (-1, -2, 'black'), (0, -2, 'shadow'), (1, -2, 'black'),
        # Body
        (-2, 0, 'black'), (-1, 0, 'shadow'), (0, 0, 'black'), (1, 0, 'shadow'), (2, 0, 'black'),
        # Legs
        (-2, 2, 'black'), (2, 2, 'black'),
        # Tail
        (2, -1, 'black'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_shieldbearer_idle(frame=0):
    img, draw = create_sprite()
    # Armored warrior with shield
    pixels = [
        # Head
        (0, -3, 'skin'),
        # Shield (large)
        (-3, -1, 'armor'), (-2, -1, 'armor_dark'), (-1, -1, 'armor'), (0, -1, 'armor_dark'),
        (-3, 0, 'armor_dark'), (-2, 0, 'armor'), (-1, 0, 'armor_dark'), (0, 0, 'armor'),
        (-3, 1, 'armor'), (-2, 1, 'armor_dark'), (-1, 1, 'armor'), (0, 1, 'armor_dark'),
        # Body
        (0, 0, 'armor'),
        # Legs
        (-1, 2, 'armor'), (1, 2, 'armor'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_pyromancer_idle(frame=0):
    img, draw = create_sprite()
    # Fire mage
    pixels = [
        # Hood
        (0, -3, 'fire_red'),
        (-1, -2, 'fire_red'), (0, -2, 'fire_orange'), (1, -2, 'fire_red'),
        # Robes
        (-1, 0, 'fire_red'), (0, 0, 'fire_orange'), (1, 0, 'fire_red'),
        # Staff
        (0, 2, 'wood'), (0, 3, 'wood'), (0, 4, 'fire_yellow'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_necromancer_idle(frame=0):
    img, draw = create_sprite()
    # Dark mage with skull mask
    pixels = [
        # Skull mask
        (0, -3, 'bone'),
        (-1, -2, 'black'), (0, -2, 'bone'), (1, -2, 'black'),
        # Robes
        (-1, 0, 'black'), (0, 0, 'magic_purple'), (1, 0, 'black'),
        # Staff
        (0, 2, 'bone'), (0, 3, 'magic_purple'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_skeleton_minion_idle(frame=0):
    img, draw = create_sprite()
    # Small skeleton
    pixels = [
        # Skull
        (0, -2, 'bone'),
        # Body
        (0, 0, 'bone_dark'),
        # Legs
        (-1, 2, 'bone'), (1, 2, 'bone'),
        # Dagger
        (0, 3, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_boulder_ram_crew_idle(frame=0):
    img, draw = create_sprite()
    # Two soldiers pushing ram
    pixels = [
        # Left soldier
        (-3, -1, 'skin'), (-3, 0, 'leather'), (-3, 1, 'leather'),
        # Right soldier
        (3, -1, 'skin'), (3, 0, 'leather'), (3, 1, 'leather'),
        # Ram
        (-2, 0, 'wood'), (-1, 0, 'iron_dark'), (0, 0, 'wood'), (1, 0, 'iron_dark'), (2, 0, 'wood'),
        (-1, 1, 'wood'), (0, 1, 'wood'), (1, 1, 'wood'),
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# Bosses
# ============================================================================

def generate_ironback_minotaur_idle(frame=0):
    img, draw = create_sprite()
    # Massive armored beast
    pixels = [
        # Head
        (0, -5, 'orc_skin_dark'),
        (-1, -4, 'orc_skin'), (0, -4, 'red'), (1, -4, 'orc_skin'),
        # Horns
        (-2, -4, 'orc_skin_dark'), (2, -4, 'orc_skin_dark'),
        # Armored body
        (-3, -2, 'armor'), (-2, -2, 'armor_dark'), (-1, -2, 'armor'), (0, -2, 'red'),
        (1, -2, 'armor'), (2, -2, 'armor_dark'), (3, -2, 'armor'),
        (-3, -1, 'armor_dark'), (-2, -1, 'armor'), (-1, -1, 'armor_dark'), (0, -1, 'red'),
        (1, -1, 'armor_dark'), (2, -1, 'armor'), (3, -1, 'armor_dark'),
        # Legs
        (-1, 2, 'armor'), (1, 2, 'armor'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_fire_drake_idle(frame=0):
    img, draw = create_sprite()
    # Wingless lava dragon
    pixels = [
        # Head
        (0, -4, 'fire_orange'),
        (-1, -3, 'fire_red'), (0, -3, 'fire_yellow'), (1, -3, 'fire_red'),
        # Body
        (-2, -1, 'fire_red'), (-1, -1, 'fire_orange'), (0, -1, 'fire_yellow'),
        (1, -1, 'fire_orange'), (2, -1, 'fire_red'),
        (-2, 0, 'fire_orange'), (-1, 0, 'fire_red'), (0, 0, 'fire_yellow'),
        (1, 0, 'fire_red'), (2, 0, 'fire_orange'),
        # Legs
        (-1, 2, 'fire_red'), (1, 2, 'fire_red'),
        # Tail
        (2, 1, 'fire_orange'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_lich_king_idle(frame=0):
    img, draw = create_sprite()
    # Frost skeletal mage
    pixels = [
        # Crown/head
        (0, -4, 'ice_blue'),
        (-1, -3, 'ice_light'), (0, -3, 'ice_white'), (1, -3, 'ice_light'),
        # Robes
        (-2, -1, 'black'), (-1, -1, 'ice_blue'), (0, -1, 'ice_light'), (1, -1, 'ice_blue'), (2, -1, 'black'),
        (-2, 0, 'black'), (-1, 0, 'ice_blue'), (0, 0, 'ice_white'), (1, 0, 'ice_blue'), (2, 0, 'black'),
        # Staff
        (0, 2, 'ice_blue'), (0, 3, 'ice_white'),
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# Projectiles
# ============================================================================

def generate_arrow_projectile(frame=0):
    img, draw = create_sprite()
    # Small arrow
    pixels = [
        (0, -1, 'wood'), (0, 0, 'wood_dark'), (0, 1, 'wood'),
        (0, 2, 'iron'),  # Arrowhead
    ]
    draw_pixels(draw, pixels)
    return img

def generate_bolt_projectile(frame=0):
    img, draw = create_sprite()
    # Crossbow bolt
    pixels = [
        (0, -1, 'iron'), (0, 0, 'iron_dark'), (0, 1, 'iron'),
        (0, 2, 'iron'),  # Bolt tip
    ]
    draw_pixels(draw, pixels)
    return img

def generate_rock_projectile(frame=0):
    img, draw = create_sprite()
    # Catapult rock
    pixels = [
        (-1, -1, 'stone'), (0, -1, 'stone_dark'), (1, -1, 'stone'),
        (-1, 0, 'stone_dark'), (0, 0, 'stone'), (1, 0, 'stone_dark'),
        (-1, 1, 'stone'), (0, 1, 'stone_dark'), (1, 1, 'stone'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_fireball_projectile(frame=0):
    img, draw = create_sprite()
    # Fireball
    pixels = [
        (-1, 0, 'fire_orange'), (0, 0, 'fire_yellow'), (1, 0, 'fire_orange'),
        (0, -1, 'fire_red'), (0, 1, 'fire_red'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_lightning_arc(frame=0):
    img, draw = create_sprite()
    # Lightning bolt
    pixels = [
        (-1, 0, 'magic_light_blue'), (0, 0, 'white'), (1, 0, 'magic_light_blue'),
        (0, -1, 'magic_blue'), (0, 1, 'magic_blue'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_poison_glob(frame=0):
    img, draw = create_sprite()
    # Poison drop
    pixels = [
        (0, 0, 'poison_green'), (0, 1, 'poison_dark_green'),
        (-1, 1, 'poison_green'), (1, 1, 'poison_green'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_cannonball(frame=0):
    img, draw = create_sprite()
    # Iron sphere
    pixels = [
        (-1, -1, 'iron_dark'), (0, -1, 'iron'), (1, -1, 'iron_dark'),
        (-1, 0, 'iron'), (0, 0, 'iron_dark'), (1, 0, 'iron'),
        (-1, 1, 'iron_dark'), (0, 1, 'iron'), (1, 1, 'iron_dark'),
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# UI Icons
# ============================================================================

def generate_ui_coin():
    img, draw = create_sprite()
    # Gold coin
    pixels = [
        (-1, -1, 'gold'), (0, -1, 'gold'), (1, -1, 'gold'),
        (-1, 0, 'gold'), (0, 0, 'fire_yellow'), (1, 0, 'gold'),
        (-1, 1, 'gold'), (0, 1, 'gold'), (1, 1, 'gold'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_ui_heart():
    img, draw = create_sprite()
    # Heart icon
    pixels = [
        (0, -2, 'red'), (-1, -1, 'red'), (0, -1, 'red'), (1, -1, 'red'),
        (-1, 0, 'red'), (0, 0, 'red'), (1, 0, 'red'),
        (0, 1, 'red'),
    ]
    draw_pixels(draw, pixels)
    return img

def generate_ui_shield():
    img, draw = create_sprite()
    # Shield icon
    pixels = [
        (0, -2, 'armor'),
        (-1, -1, 'armor'), (0, -1, 'blue'), (1, -1, 'armor'),
        (-1, 0, 'armor'), (0, 0, 'blue'), (1, 0, 'armor'),
        (-1, 1, 'armor'), (0, 1, 'blue'), (1, 1, 'armor'),
        (0, 2, 'armor'),
    ]
    draw_pixels(draw, pixels)
    return img

# ============================================================================
# Main Generation Function
# ============================================================================

def generate_all_assets():
    """Generate all pixel art assets"""
    
    print("Generating pixel art assets...\n")
    
    # Environment tiles
    env_path = "assets/images/environment"
    save_sprite(generate_castle_wall_horizontal(), f"{env_path}/castle_wall_straight_horizontal.png")
    save_sprite(generate_castle_wall_vertical(), f"{env_path}/castle_wall_straight_vertical.png")
    save_sprite(generate_castle_wall_corner_NE(), f"{env_path}/castle_wall_corner_NE.png")
    save_sprite(generate_castle_wall_corner_NW(), f"{env_path}/castle_wall_corner_NW.png")
    save_sprite(generate_castle_wall_corner_SE(), f"{env_path}/castle_wall_corner_SE.png")
    save_sprite(generate_castle_wall_corner_SW(), f"{env_path}/castle_wall_corner_SW.png")
    save_sprite(generate_castle_gate_closed(), f"{env_path}/castle_gate_closed.png")
    save_sprite(generate_castle_gate_open_top(), f"{env_path}/castle_gate_open_top.png")
    save_sprite(generate_castle_gate_open_bottom(), f"{env_path}/castle_gate_open_bottom.png")
    save_sprite(generate_castle_floor_stone(), f"{env_path}/castle_floor_stone.png")
    
    for variant in ['A', 'B', 'C']:
        save_sprite(generate_grass_tile(variant), f"{env_path}/grass_tile_{variant}.png")
        save_sprite(generate_path_tile(variant), f"{env_path}/path_tile_{variant}.png")
    
    # Towers
    towers = {
        'arrow_tower': (generate_arrow_tower_idle, generate_arrow_tower_attack, generate_arrow_tower_death),
        'catapult_tower': (generate_catapult_tower_idle, generate_catapult_tower_attack, generate_catapult_tower_idle),
        'crossbow_tower': (generate_crossbow_tower_idle, generate_crossbow_tower_idle, generate_crossbow_tower_idle),
        'fire_tower': (generate_fire_tower_idle, generate_fire_tower_attack, generate_fire_tower_idle),
        'tesla_tower': (generate_tesla_tower_idle, generate_tesla_tower_attack, generate_tesla_tower_idle),
        'ballista_tower': (generate_ballista_tower_idle, generate_ballista_tower_idle, generate_ballista_tower_idle),
        'poison_tower': (generate_poison_tower_idle, generate_poison_tower_idle, generate_poison_tower_idle),
        'bombard_tower': (generate_bombard_tower_idle, generate_bombard_tower_idle, generate_bombard_tower_idle),
    }
    
    for name, (idle_fn, attack_fn, death_fn) in towers.items():
        tower_path = f"assets/images/towers/{name}"
        for frame in range(3):
            save_sprite(idle_fn(frame), f"{tower_path}/idle/{frame}.png")
        for frame in range(5):
            save_sprite(attack_fn(frame), f"{tower_path}/attack/{frame}.png")
        for frame in range(4):
            save_sprite(death_fn(frame), f"{tower_path}/death/{frame}.png")
    
    # Traps
    traps = {
        'spike_trap': (generate_spike_trap_idle, generate_spike_trap_trigger),
        'freeze_trap': (generate_freeze_trap_idle, generate_freeze_trap_idle),
        'fire_pit_trap': (generate_fire_pit_trap_idle, generate_fire_pit_trap_idle),
        'magic_snare_trap': (generate_magic_snare_trap_idle, generate_magic_snare_trap_idle),
        'explosive_barrel': (generate_explosive_barrel_idle, generate_explosive_barrel_idle),
    }
    
    for name, (idle_fn, trigger_fn) in traps.items():
        trap_path = f"assets/images/traps/{name}"
        for frame in range(2):
            save_sprite(idle_fn(frame), f"{trap_path}/idle/{frame}.png")
        for frame in range(4):
            save_sprite(trigger_fn(frame), f"{trap_path}/trigger/{frame}.png")
        for frame in range(3):
            save_sprite(idle_fn(frame), f"{trap_path}/destroy/{frame}.png")
    
    # Enemies
    enemies = {
        'grunt_raider': (generate_grunt_raider_idle, generate_grunt_raider_move, generate_grunt_raider_attack),
        'brute_crusher': (generate_brute_crusher_idle, generate_brute_crusher_idle, generate_brute_crusher_idle),
        'direwolf': (generate_direwolf_idle, generate_direwolf_idle, generate_direwolf_idle),
        'shieldbearer': (generate_shieldbearer_idle, generate_shieldbearer_idle, generate_shieldbearer_idle),
        'pyromancer': (generate_pyromancer_idle, generate_pyromancer_idle, generate_pyromancer_idle),
        'necromancer': (generate_necromancer_idle, generate_necromancer_idle, generate_necromancer_idle),
        'skeleton_minion': (generate_skeleton_minion_idle, generate_skeleton_minion_idle, generate_skeleton_minion_idle),
        'boulder_ram_crew': (generate_boulder_ram_crew_idle, generate_boulder_ram_crew_idle, generate_boulder_ram_crew_idle),
    }
    
    for name, (idle_fn, move_fn, attack_fn) in enemies.items():
        enemy_path = f"assets/images/enemies/{name}"
        for frame in range(3):
            save_sprite(idle_fn(frame), f"{enemy_path}/idle/{frame}.png")
        for frame in range(6):
            save_sprite(move_fn(frame), f"{enemy_path}/move/{frame}.png")
        for frame in range(5):
            save_sprite(attack_fn(frame), f"{enemy_path}/attack/{frame}.png")
        for frame in range(5):
            save_sprite(idle_fn(frame), f"{enemy_path}/death/{frame}.png")
    
    # Bosses
    bosses = {
        'ironback_minotaur': (generate_ironback_minotaur_idle, generate_ironback_minotaur_idle, generate_ironback_minotaur_idle),
        'fire_drake': (generate_fire_drake_idle, generate_fire_drake_idle, generate_fire_drake_idle),
        'lich_king_arcthros': (generate_lich_king_idle, generate_lich_king_idle, generate_lich_king_idle),
    }
    
    for name, (idle_fn, move_fn, attack_fn) in bosses.items():
        boss_path = f"assets/images/bosses/{name}"
        for frame in range(3):
            save_sprite(idle_fn(frame), f"{boss_path}/idle/{frame}.png")
        for frame in range(6):
            save_sprite(move_fn(frame), f"{boss_path}/move/{frame}.png")
        for frame in range(6):
            save_sprite(attack_fn(frame), f"{boss_path}/attack/{frame}.png")
        for frame in range(6):
            save_sprite(idle_fn(frame), f"{boss_path}/death/{frame}.png")
    
    # Projectiles
    projectiles = {
        'arrow_projectile': generate_arrow_projectile,
        'bolt_projectile': generate_bolt_projectile,
        'rock_projectile': generate_rock_projectile,
        'fireball_projectile': generate_fireball_projectile,
        'lightning_arc': generate_lightning_arc,
        'poison_glob': generate_poison_glob,
        'cannonball': generate_cannonball,
    }
    
    for name, gen_fn in projectiles.items():
        proj_path = f"assets/images/projectiles/{name}"
        for frame in range(3):
            save_sprite(gen_fn(frame), f"{proj_path}/flying/{frame}.png")
    
    # UI Icons
    ui_path = "assets/images/ui"
    save_sprite(generate_ui_coin(), f"{ui_path}/ui_coin.png")
    save_sprite(generate_ui_heart(), f"{ui_path}/ui_heart.png")
    save_sprite(generate_ui_shield(), f"{ui_path}/ui_shield.png")
    
    print("\n✅ All assets generated successfully!")

if __name__ == "__main__":
    try:
        generate_all_assets()
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
