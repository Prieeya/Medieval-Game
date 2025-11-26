#!/usr/bin/env node
/**
 * Pixel Art Asset Generator for Medieval Siege Simulator
 * Generates 32x32 pixel art sprites as PNG files
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import sharp from 'sharp';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Color palette (RGB values)
const COLORS = {
    stone_light: [180, 180, 200], stone: [140, 140, 160], stone_dark: [100, 100, 120],
    iron: [200, 200, 220], iron_dark: [120, 120, 140],
    wood_light: [180, 140, 100], wood: [140, 100, 60], wood_dark: [100, 70, 40],
    grass_light: [120, 180, 100], grass: [80, 140, 60], grass_dark: [60, 100, 40],
    dirt_light: [160, 120, 80], dirt: [120, 80, 50], dirt_dark: [80, 60, 40],
    fire_yellow: [255, 255, 100], fire_orange: [255, 180, 60], fire_red: [220, 60, 40],
    magic_blue: [100, 150, 255], magic_light_blue: [150, 200, 255],
    magic_purple: [180, 100, 220], magic_light_purple: [220, 150, 255],
    ice_blue: [150, 200, 255], ice_light: [200, 230, 255], ice_white: [255, 255, 255],
    poison_green: [60, 200, 100], poison_dark_green: [40, 140, 60],
    skin_light: [255, 220, 180], skin: [220, 180, 140], skin_dark: [180, 140, 100],
    leather: [100, 80, 60], armor: [180, 180, 200], armor_dark: [120, 120, 140],
    orc_skin: [100, 140, 100], orc_skin_dark: [60, 100, 60],
    black: [20, 20, 20], white: [255, 255, 255], red: [220, 60, 60], blue: [60, 100, 220],
    gold: [255, 220, 100], shadow: [40, 40, 60], green: [60, 180, 100],
    bone: [240, 240, 220], bone_dark: [200, 200, 180],
};

function getColor(name) {
    return COLORS[name] || COLORS.black;
}

// Sprite sizes: Enemies/Towers 48x48, Projectiles 24x24, Tiles 32x32
const ENEMY_SIZE = 48;
const TOWER_SIZE = 48;
const PROJECTILE_SIZE = 24;
const TILE_SIZE = 32;

// Helper to add outline/shadow to sprites for better visibility
function addOutline(pixels, size, outlineColor = COLORS.black) {
    const outlined = createPixelArray(size);
    // Copy original pixels
    for (let i = 0; i < pixels.length; i++) {
        outlined[i] = pixels[i];
    }
    // Add outline around non-transparent pixels
    for (let y = 0; y < size; y++) {
        for (let x = 0; x < size; x++) {
            const idx = (y * size + x) * 4;
            if (pixels[idx + 3] > 0) { // If pixel is not transparent
                // Check neighbors
                const neighbors = [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]];
                for (const [dx, dy] of neighbors) {
                    const nx = x + dx;
                    const ny = y + dy;
                    if (nx >= 0 && nx < size && ny >= 0 && ny < size) {
                        const nIdx = (ny * size + nx) * 4;
                        if (pixels[nIdx + 3] === 0) { // If neighbor is transparent
                            setPixel(outlined, nx, ny, outlineColor, size);
                        }
                    }
                }
            }
        }
    }
    return outlined;
}

// Helper to add white/yellow outline for projectiles
function addProjectileOutline(pixels, size) {
    const outlined = createPixelArray(size);
    // Copy original pixels
    for (let i = 0; i < pixels.length; i++) {
        outlined[i] = pixels[i];
    }
    // Add bright outline for visibility
    for (let y = 0; y < size; y++) {
        for (let x = 0; x < size; x++) {
            const idx = (y * size + x) * 4;
            if (pixels[idx + 3] > 0) { // If pixel is not transparent
                // Check neighbors
                const neighbors = [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]];
                for (const [dx, dy] of neighbors) {
                    const nx = x + dx;
                    const ny = y + dy;
                    if (nx >= 0 && nx < size && ny >= 0 && ny < size) {
                        const nIdx = (ny * size + nx) * 4;
                        if (pixels[nIdx + 3] === 0) { // If neighbor is transparent
                            // Use white or yellow outline
                            const outlineCol = (x + y) % 2 === 0 ? COLORS.white : COLORS.fire_yellow;
                            setPixel(outlined, nx, ny, outlineCol, size);
                        }
                    }
                }
            }
        }
    }
    return outlined;
}

function createPixelArray(size = 32) {
    // Create RGBA array (transparent by default)
    const pixels = new Uint8ClampedArray(size * size * 4);
    for (let i = 0; i < pixels.length; i += 4) {
        pixels[i] = 0;     // R
        pixels[i + 1] = 0; // G
        pixels[i + 2] = 0; // B
        pixels[i + 3] = 0; // A (transparent)
    }
    return pixels;
}

function setPixel(pixels, x, y, color, size = 32) {
    if (x >= 0 && x < size && y >= 0 && y < size) {
        const idx = (y * size + x) * 4;
        pixels[idx] = color[0];     // R
        pixels[idx + 1] = color[1]; // G
        pixels[idx + 2] = color[2]; // B
        pixels[idx + 3] = 255;      // A (opaque)
    }
}

// Draw outline/shadow for better visibility
function setPixelWithOutline(pixels, x, y, color, size, outlineColor = null) {
    setPixel(pixels, x, y, color, size);
    if (outlineColor) {
        // Add outline pixels around edges
        const neighbors = [[-1,0], [1,0], [0,-1], [0,1]];
        for (const [dx, dy] of neighbors) {
            const nx = x + dx;
            const ny = y + dy;
            if (nx >= 0 && nx < size && ny >= 0 && ny < size) {
                const idx = (ny * size + nx) * 4;
                if (pixels[idx + 3] === 0) { // Only if transparent
                    setPixel(pixels, nx, ny, outlineColor, size);
                }
            }
        }
    }
}

function drawPixels(pixels, pixelList, offsetX, offsetY, size, withOutline = false) {
    const outlineColor = withOutline ? getColor('black') : null;
    for (const [x, y, colorName] of pixelList) {
        if (withOutline) {
            setPixelWithOutline(pixels, offsetX + x, offsetY + y, getColor(colorName), size, outlineColor);
        } else {
            setPixel(pixels, offsetX + x, offsetY + y, getColor(colorName), size);
        }
    }
}

async function saveSprite(pixels, filePath, size = 32) {
    // Ensure directory exists
    const dir = path.dirname(filePath);
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
    }
    
    // Create image buffer from pixel data
    const imageBuffer = await sharp(Buffer.from(pixels), {
        raw: {
            width: size,
            height: size,
            channels: 4
        }
    })
    .png()
    .toBuffer();
    
    fs.writeFileSync(filePath, imageBuffer);
    console.log(`✓ ${filePath}`);
}

// ============================================================================
// Environment Tiles
// ============================================================================

function generateCastleWallHorizontal() {
    const pixels = createPixelArray();
    // Stone wall horizontal
    for (let y = 10; y < 22; y++) {
        for (let x = 2; x < 30; x++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    // Top edge shadow
    for (let x = 2; x < 30; x++) {
        setPixel(pixels, x, 10, getColor('stone_dark'));
    }
    // Mortar lines
    for (let x = 2; x < 30; x += 4) {
        setPixel(pixels, x, 14, getColor('stone_dark'));
        setPixel(pixels, x, 18, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleWallVertical() {
    const pixels = createPixelArray();
    // Stone wall vertical
    for (let x = 10; x < 22; x++) {
        for (let y = 2; y < 30; y++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    // Left edge shadow
    for (let y = 2; y < 30; y++) {
        setPixel(pixels, 10, y, getColor('stone_dark'));
    }
    // Mortar lines
    for (let y = 2; y < 30; y += 4) {
        setPixel(pixels, 14, y, getColor('stone_dark'));
        setPixel(pixels, 18, y, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleWallCornerNE() {
    const pixels = createPixelArray();
    // Corner piece - Northeast
    for (let y = 10; y < 22; y++) {
        for (let x = 10; x < 22; x++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    // Edges
    for (let x = 10; x < 22; x++) {
        setPixel(pixels, x, 10, getColor('stone_dark'));
    }
    for (let y = 10; y < 22; y++) {
        setPixel(pixels, 21, y, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleWallCornerNW() {
    const pixels = createPixelArray();
    for (let y = 10; y < 22; y++) {
        for (let x = 10; x < 22; x++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    for (let x = 10; x < 22; x++) {
        setPixel(pixels, x, 10, getColor('stone_dark'));
    }
    for (let y = 10; y < 22; y++) {
        setPixel(pixels, 10, y, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleWallCornerSE() {
    const pixels = createPixelArray();
    for (let y = 10; y < 22; y++) {
        for (let x = 10; x < 22; x++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    for (let x = 10; x < 22; x++) {
        setPixel(pixels, x, 21, getColor('stone_dark'));
    }
    for (let y = 10; y < 22; y++) {
        setPixel(pixels, 21, y, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleWallCornerSW() {
    const pixels = createPixelArray();
    for (let y = 10; y < 22; y++) {
        for (let x = 10; x < 22; x++) {
            const color = (x + y) % 3 === 0 ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    for (let x = 10; x < 22; x++) {
        setPixel(pixels, x, 21, getColor('stone_dark'));
    }
    for (let y = 10; y < 22; y++) {
        setPixel(pixels, 10, y, getColor('stone_dark'));
    }
    return pixels;
}

function generateCastleGateClosed() {
    const pixels = createPixelArray();
    // Wooden gate - closed
    for (let y = 8; y < 24; y++) {
        for (let x = 8; x < 24; x++) {
            const color = (x + y) % 2 === 0 ? getColor('wood') : getColor('wood_dark');
            setPixel(pixels, x, y, color);
        }
    }
    // Iron bands
    for (let x = 8; x < 24; x++) {
        setPixel(pixels, x, 12, getColor('iron_dark'));
        setPixel(pixels, x, 16, getColor('iron_dark'));
        setPixel(pixels, x, 20, getColor('iron_dark'));
    }
    // Iron hinges
    setPixel(pixels, 8, 14, getColor('iron'));
    setPixel(pixels, 8, 18, getColor('iron'));
    setPixel(pixels, 23, 14, getColor('iron'));
    setPixel(pixels, 23, 18, getColor('iron'));
    return pixels;
}

function generateCastleGateOpenTop() {
    const pixels = createPixelArray();
    // Gate open - top half
    for (let y = 8; y < 16; y++) {
        for (let x = 8; x < 24; x++) {
            const color = (x + y) % 2 === 0 ? getColor('wood') : getColor('wood_dark');
            setPixel(pixels, x, y, color);
        }
    }
    // Iron band
    for (let x = 8; x < 24; x++) {
        setPixel(pixels, x, 12, getColor('iron_dark'));
    }
    return pixels;
}

function generateCastleGateOpenBottom() {
    const pixels = createPixelArray();
    // Gate open - bottom half
    for (let y = 16; y < 24; y++) {
        for (let x = 8; x < 24; x++) {
            const color = (x + y) % 2 === 0 ? getColor('wood') : getColor('wood_dark');
            setPixel(pixels, x, y, color);
        }
    }
    // Iron bands
    for (let x = 8; x < 24; x++) {
        setPixel(pixels, x, 16, getColor('iron_dark'));
        setPixel(pixels, x, 20, getColor('iron_dark'));
    }
    return pixels;
}

function generateCastleFloorStone() {
    const pixels = createPixelArray();
    // Stone floor tiles
    for (let y = 0; y < 32; y++) {
        for (let x = 0; x < 32; x++) {
            const color = (Math.floor(x / 4) + Math.floor(y / 4)) % 2 === 0 
                ? getColor('stone_light') : getColor('stone');
            setPixel(pixels, x, y, color);
        }
    }
    // Grid lines
    for (let x = 0; x < 32; x += 4) {
        for (let y = 0; y < 32; y++) {
            setPixel(pixels, x, y, getColor('stone_dark'));
        }
    }
    for (let y = 0; y < 32; y += 4) {
        for (let x = 0; x < 32; x++) {
            setPixel(pixels, x, y, getColor('stone_dark'));
        }
    }
    return pixels;
}

function generateGrassTile(variant = 'A') {
    const pixels = createPixelArray();
    // Base grass
    for (let y = 0; y < 32; y++) {
        for (let x = 0; x < 32; x++) {
            const color = (x + y) % 2 === 0 ? getColor('grass') : getColor('grass_light');
            setPixel(pixels, x, y, color);
        }
    }
    // Add small flowers for variation
    if (variant === 'A') {
        setPixel(pixels, 8, 8, getColor('fire_yellow'));
        setPixel(pixels, 24, 20, getColor('fire_yellow'));
    } else if (variant === 'B') {
        setPixel(pixels, 12, 16, getColor('magic_purple'));
        setPixel(pixels, 20, 8, getColor('magic_purple'));
    } else if (variant === 'C') {
        setPixel(pixels, 16, 12, getColor('fire_yellow'));
        setPixel(pixels, 6, 24, getColor('magic_purple'));
    }
    return pixels;
}

function generatePathTile(variant = 'A') {
    const pixels = createPixelArray();
    // Dirt path
    for (let y = 0; y < 32; y++) {
        for (let x = 0; x < 32; x++) {
            const color = (x + y) % 2 === 0 ? getColor('dirt') : getColor('dirt_light');
            setPixel(pixels, x, y, color);
        }
    }
    // Add pebbles
    if (variant === 'A') {
        setPixel(pixels, 10, 10, getColor('stone'));
        setPixel(pixels, 22, 18, getColor('stone'));
    } else if (variant === 'B') {
        setPixel(pixels, 14, 8, getColor('stone'));
        setPixel(pixels, 18, 24, getColor('stone'));
    } else if (variant === 'C') {
        setPixel(pixels, 8, 20, getColor('stone'));
        setPixel(pixels, 24, 12, getColor('stone'));
    }
    return pixels;
}

// ============================================================================
// Towers
// ============================================================================

function generateArrowTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Base - larger and more detailed for 48x48
        [-7, 10, 'wood'], [-6, 10, 'wood_dark'], [-5, 10, 'wood'], [-4, 10, 'wood_dark'],
        [-3, 10, 'wood'], [-2, 10, 'wood_dark'], [-1, 10, 'wood'], [0, 10, 'wood_dark'],
        [1, 10, 'wood'], [2, 10, 'wood_dark'], [3, 10, 'wood'], [4, 10, 'wood_dark'],
        [5, 10, 'wood'], [6, 10, 'wood_dark'], [7, 10, 'wood'],
        [-6, 11, 'wood_dark'], [-5, 11, 'wood'], [-4, 11, 'wood_dark'], [-3, 11, 'wood'],
        [-2, 11, 'wood_dark'], [-1, 11, 'wood'], [0, 11, 'wood_dark'], [1, 11, 'wood'],
        [2, 11, 'wood_dark'], [3, 11, 'wood'], [4, 11, 'wood_dark'], [5, 11, 'wood'], [6, 11, 'wood_dark'],
        [-5, 12, 'wood'], [-4, 12, 'wood_dark'], [-3, 12, 'wood'], [-2, 12, 'wood_dark'],
        [-1, 12, 'wood'], [0, 12, 'wood_dark'], [1, 12, 'wood'], [2, 12, 'wood_dark'],
        [3, 12, 'wood'], [4, 12, 'wood_dark'], [5, 12, 'wood'],
        // Tower body - taller and more detailed
        [-4, 6, 'wood'], [-3, 6, 'wood_dark'], [-2, 6, 'wood'], [-1, 6, 'wood_dark'],
        [0, 6, 'wood'], [1, 6, 'wood_dark'], [2, 6, 'wood'], [3, 6, 'wood_dark'], [4, 6, 'wood'],
        [-4, 7, 'wood_dark'], [-3, 7, 'wood'], [-2, 7, 'wood_dark'], [-1, 7, 'wood'],
        [0, 7, 'wood_dark'], [1, 7, 'wood'], [2, 7, 'wood_dark'], [3, 7, 'wood'], [4, 7, 'wood_dark'],
        [-4, 8, 'wood'], [-3, 8, 'wood_dark'], [-2, 8, 'wood'], [-1, 8, 'wood_dark'],
        [0, 8, 'wood'], [1, 8, 'wood_dark'], [2, 8, 'wood'], [3, 8, 'wood_dark'], [4, 8, 'wood'],
        [-4, 9, 'wood_dark'], [-3, 9, 'wood'], [-2, 9, 'wood_dark'], [-1, 9, 'wood'],
        [0, 9, 'wood_dark'], [1, 9, 'wood'], [2, 9, 'wood_dark'], [3, 9, 'wood'], [4, 9, 'wood_dark'],
        // Platform
        [-5, 5, 'wood'], [-4, 5, 'wood_dark'], [-3, 5, 'wood'], [-2, 5, 'wood_dark'],
        [-1, 5, 'wood'], [0, 5, 'wood_dark'], [1, 5, 'wood'], [2, 5, 'wood_dark'],
        [3, 5, 'wood'], [4, 5, 'wood_dark'], [5, 5, 'wood'],
        // Archer on top - more detailed
        [0, 3, 'skin'], [-1, 2, 'leather'], [0, 2, 'skin'], [1, 2, 'leather'],
        [-1, 1, 'leather'], [0, 1, 'skin'], [1, 1, 'leather'],
        [0, 0, 'leather'], [-1, -1, 'leather'], [0, -1, 'skin'], [1, -1, 'leather'],
        [0, -2, 'leather'], [-1, -3, 'leather'], [0, -3, 'skin'], [1, -3, 'leather'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateArrowTowerAttack(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Base (same as idle)
        [-7, 10, 'wood'], [-6, 10, 'wood_dark'], [-5, 10, 'wood'], [-4, 10, 'wood_dark'],
        [-3, 10, 'wood'], [-2, 10, 'wood_dark'], [-1, 10, 'wood'], [0, 10, 'wood_dark'],
        [1, 10, 'wood'], [2, 10, 'wood_dark'], [3, 10, 'wood'], [4, 10, 'wood_dark'],
        [5, 10, 'wood'], [6, 10, 'wood_dark'], [7, 10, 'wood'],
        [-6, 11, 'wood_dark'], [-5, 11, 'wood'], [-4, 11, 'wood_dark'], [-3, 11, 'wood'],
        [-2, 11, 'wood_dark'], [-1, 11, 'wood'], [0, 11, 'wood_dark'], [1, 11, 'wood'],
        [2, 11, 'wood_dark'], [3, 11, 'wood'], [4, 11, 'wood_dark'], [5, 11, 'wood'], [6, 11, 'wood_dark'],
        [-5, 12, 'wood'], [-4, 12, 'wood_dark'], [-3, 12, 'wood'], [-2, 12, 'wood_dark'],
        [-1, 12, 'wood'], [0, 12, 'wood_dark'], [1, 12, 'wood'], [2, 12, 'wood_dark'],
        [3, 12, 'wood'], [4, 12, 'wood_dark'], [5, 12, 'wood'],
        // Tower body
        [-4, 6, 'wood'], [-3, 6, 'wood_dark'], [-2, 6, 'wood'], [-1, 6, 'wood_dark'],
        [0, 6, 'wood'], [1, 6, 'wood_dark'], [2, 6, 'wood'], [3, 6, 'wood_dark'], [4, 6, 'wood'],
        [-4, 7, 'wood_dark'], [-3, 7, 'wood'], [-2, 7, 'wood_dark'], [-1, 7, 'wood'],
        [0, 7, 'wood_dark'], [1, 7, 'wood'], [2, 7, 'wood_dark'], [3, 7, 'wood'], [4, 7, 'wood_dark'],
        [-4, 8, 'wood'], [-3, 8, 'wood_dark'], [-2, 8, 'wood'], [-1, 8, 'wood_dark'],
        [0, 8, 'wood'], [1, 8, 'wood_dark'], [2, 8, 'wood'], [3, 8, 'wood_dark'], [4, 8, 'wood'],
        [-4, 9, 'wood_dark'], [-3, 9, 'wood'], [-2, 9, 'wood_dark'], [-1, 9, 'wood'],
        [0, 9, 'wood_dark'], [1, 9, 'wood'], [2, 9, 'wood_dark'], [3, 9, 'wood'], [4, 9, 'wood_dark'],
        // Platform
        [-5, 5, 'wood'], [-4, 5, 'wood_dark'], [-3, 5, 'wood'], [-2, 5, 'wood_dark'],
        [-1, 5, 'wood'], [0, 5, 'wood_dark'], [1, 5, 'wood'], [2, 5, 'wood_dark'],
        [3, 5, 'wood'], [4, 5, 'wood_dark'], [5, 5, 'wood'],
        // Archer shooting (bow extended)
        [0, 3, 'skin'], [-1, 2, 'leather'], [0, 2, 'skin'], [1, 2, 'leather'],
        [-1, 1, 'leather'], [0, 1, 'skin'], [1, 1, 'leather'],
        [0, 0, 'leather'], [2, 1, 'wood'], [3, 1, 'wood'], [4, 1, 'wood'], [5, 1, 'wood'],  // Bow and arrow
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateArrowTowerDeath(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Collapsed tower
        [-4, 10, 'wood_dark'], [-3, 10, 'wood'], [-2, 10, 'wood_dark'], [-1, 10, 'wood'],
        [0, 10, 'wood_dark'], [1, 10, 'wood'], [2, 10, 'wood_dark'], [3, 10, 'wood'], [4, 10, 'wood_dark'],
        [-3, 11, 'wood'], [-2, 11, 'wood_dark'], [-1, 11, 'wood'], [0, 11, 'wood_dark'],
        [1, 11, 'wood'], [2, 11, 'wood_dark'], [3, 11, 'wood'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateCatapultTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-7, 10, 'stone'], [-6, 10, 'stone_dark'], [-5, 10, 'stone'], [-4, 10, 'stone_dark'],
        [-3, 10, 'stone'], [-2, 10, 'stone_dark'], [-1, 10, 'stone'], [0, 10, 'stone_dark'],
        [1, 10, 'stone'], [2, 10, 'stone_dark'], [3, 10, 'stone'], [4, 10, 'stone_dark'],
        [5, 10, 'stone'], [6, 10, 'stone_dark'], [7, 10, 'stone'],
        [-6, 11, 'stone_dark'], [-5, 11, 'stone'], [-4, 11, 'stone_dark'], [-3, 11, 'stone'],
        [-2, 11, 'stone_dark'], [-1, 11, 'stone'], [0, 11, 'stone_dark'], [1, 11, 'stone'],
        [2, 11, 'stone_dark'], [3, 11, 'stone'], [4, 11, 'stone_dark'], [5, 11, 'stone'], [6, 11, 'stone_dark'],
        // Catapult frame
        [-4, 6, 'wood'], [-3, 6, 'wood_dark'], [-2, 6, 'wood'], [-1, 6, 'wood_dark'],
        [0, 6, 'wood'], [1, 6, 'wood_dark'], [2, 6, 'wood'], [3, 6, 'wood_dark'], [4, 6, 'wood'],
        // Catapult arm
        [-3, 3, 'wood'], [-2, 3, 'wood_dark'], [-1, 3, 'wood'], [0, 3, 'wood_dark'],
        [1, 3, 'wood'], [2, 3, 'wood_dark'], [3, 3, 'wood'],
        [-2, 2, 'wood'], [-1, 2, 'wood_dark'], [0, 2, 'wood'], [1, 2, 'wood_dark'], [2, 2, 'wood'],
        [0, 1, 'wood'],  // Arm tip
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateCatapultTowerAttack(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-7, 10, 'stone'], [-6, 10, 'stone_dark'], [-5, 10, 'stone'], [-4, 10, 'stone_dark'],
        [-3, 10, 'stone'], [-2, 10, 'stone_dark'], [-1, 10, 'stone'], [0, 10, 'stone_dark'],
        [1, 10, 'stone'], [2, 10, 'stone_dark'], [3, 10, 'stone'], [4, 10, 'stone_dark'],
        [5, 10, 'stone'], [6, 10, 'stone_dark'], [7, 10, 'stone'],
        [-6, 11, 'stone_dark'], [-5, 11, 'stone'], [-4, 11, 'stone_dark'], [-3, 11, 'stone'],
        [-2, 11, 'stone_dark'], [-1, 11, 'stone'], [0, 11, 'stone_dark'], [1, 11, 'stone'],
        [2, 11, 'stone_dark'], [3, 11, 'stone'], [4, 11, 'stone_dark'], [5, 11, 'stone'], [6, 11, 'stone_dark'],
        // Catapult frame
        [-4, 6, 'wood'], [-3, 6, 'wood_dark'], [-2, 6, 'wood'], [-1, 6, 'wood_dark'],
        [0, 6, 'wood'], [1, 6, 'wood_dark'], [2, 6, 'wood'], [3, 6, 'wood_dark'], [4, 6, 'wood'],
        // Catapult arm (rotated back)
        [-2, 4, 'wood'], [-1, 4, 'wood_dark'], [0, 4, 'wood'], [1, 4, 'wood_dark'], [2, 4, 'wood'],
        [1, 3, 'wood'], [2, 3, 'wood_dark'], [3, 3, 'wood'],
        [2, 2, 'wood'], [3, 2, 'wood_dark'], [4, 2, 'wood'],
        [3, 1, 'wood'], [4, 1, 'wood'],  // Arm tip
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateCrossbowTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Base - scaled for 48x48
        [-6, 10, 'wood'], [-5, 10, 'wood_dark'], [-4, 10, 'wood'], [-3, 10, 'wood_dark'],
        [-2, 10, 'wood'], [-1, 10, 'wood_dark'], [0, 10, 'wood'], [1, 10, 'wood_dark'],
        [2, 10, 'wood'], [3, 10, 'wood_dark'], [4, 10, 'wood'], [5, 10, 'wood_dark'], [6, 10, 'wood'],
        [-5, 11, 'wood_dark'], [-4, 11, 'wood'], [-3, 11, 'wood_dark'], [-2, 11, 'wood'],
        [-1, 11, 'wood_dark'], [0, 11, 'wood'], [1, 11, 'wood_dark'], [2, 11, 'wood'],
        [3, 11, 'wood_dark'], [4, 11, 'wood'], [5, 11, 'wood_dark'],
        // Tower body
        [-3, 6, 'wood'], [-2, 6, 'iron'], [-1, 6, 'wood'], [0, 6, 'iron'], [1, 6, 'wood'],
        [2, 6, 'iron'], [3, 6, 'wood'],
        [-3, 7, 'iron'], [-2, 7, 'wood'], [-1, 7, 'iron'], [0, 7, 'wood'], [1, 7, 'iron'],
        [2, 7, 'wood'], [3, 7, 'iron'],
        // Crossbow
        [-4, 3, 'wood'], [-3, 3, 'iron'], [-2, 3, 'wood'], [-1, 3, 'iron'], [0, 3, 'wood'],
        [1, 3, 'iron'], [2, 3, 'wood'], [3, 3, 'iron'], [4, 3, 'wood'],
        [0, 2, 'iron'],  // Crossbow center
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateFireTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-6, 10, 'stone'], [-5, 10, 'stone_dark'], [-4, 10, 'stone'], [-3, 10, 'stone_dark'],
        [-2, 10, 'stone'], [-1, 10, 'stone_dark'], [0, 10, 'stone'], [1, 10, 'stone_dark'],
        [2, 10, 'stone'], [3, 10, 'stone_dark'], [4, 10, 'stone'], [5, 10, 'stone_dark'], [6, 10, 'stone'],
        [-5, 11, 'stone_dark'], [-4, 11, 'stone'], [-3, 11, 'stone_dark'], [-2, 11, 'stone'],
        [-1, 11, 'stone_dark'], [0, 11, 'stone'], [1, 11, 'stone_dark'], [2, 11, 'stone'],
        [3, 11, 'stone_dark'], [4, 11, 'stone'], [5, 11, 'stone_dark'],
        // Iron tower
        [-3, 6, 'iron'], [-2, 6, 'iron_dark'], [-1, 6, 'iron'], [0, 6, 'iron_dark'], [1, 6, 'iron'],
        [2, 6, 'iron_dark'], [3, 6, 'iron'],
        [-3, 7, 'iron_dark'], [-2, 7, 'iron'], [-1, 7, 'iron_dark'], [0, 7, 'iron'], [1, 7, 'iron_dark'],
        [2, 7, 'iron'], [3, 7, 'iron_dark'],
        [-3, 8, 'iron'], [-2, 8, 'iron_dark'], [-1, 8, 'iron'], [0, 8, 'iron_dark'], [1, 8, 'iron'],
        [2, 8, 'iron_dark'], [3, 8, 'iron'],
        // Brazier
        [-2, 3, 'iron_dark'], [-1, 3, 'fire_red'], [0, 3, 'iron_dark'], [1, 3, 'fire_red'], [2, 3, 'iron_dark'],
        [-1, 2, 'fire_orange'], [0, 2, 'fire_yellow'], [1, 2, 'fire_orange'],
        [0, 1, 'fire_yellow'],  // Flame
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateFireTowerAttack(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-6, 10, 'stone'], [-5, 10, 'stone_dark'], [-4, 10, 'stone'], [-3, 10, 'stone_dark'],
        [-2, 10, 'stone'], [-1, 10, 'stone_dark'], [0, 10, 'stone'], [1, 10, 'stone_dark'],
        [2, 10, 'stone'], [3, 10, 'stone_dark'], [4, 10, 'stone'], [5, 10, 'stone_dark'], [6, 10, 'stone'],
        [-5, 11, 'stone_dark'], [-4, 11, 'stone'], [-3, 11, 'stone_dark'], [-2, 11, 'stone'],
        [-1, 11, 'stone_dark'], [0, 11, 'stone'], [1, 11, 'stone_dark'], [2, 11, 'stone'],
        [3, 11, 'stone_dark'], [4, 11, 'stone'], [5, 11, 'stone_dark'],
        // Iron tower
        [-3, 6, 'iron'], [-2, 6, 'iron_dark'], [-1, 6, 'iron'], [0, 6, 'iron_dark'], [1, 6, 'iron'],
        [2, 6, 'iron_dark'], [3, 6, 'iron'],
        [-3, 7, 'iron_dark'], [-2, 7, 'iron'], [-1, 7, 'iron_dark'], [0, 7, 'iron'], [1, 7, 'iron_dark'],
        [2, 7, 'iron'], [3, 7, 'iron_dark'],
        [-3, 8, 'iron'], [-2, 8, 'iron_dark'], [-1, 8, 'iron'], [0, 8, 'iron_dark'], [1, 8, 'iron'],
        [2, 8, 'iron_dark'], [3, 8, 'iron'],
        // Larger brazier flame
        [-2, 3, 'iron_dark'], [-1, 3, 'fire_red'], [0, 3, 'iron_dark'], [1, 3, 'fire_red'], [2, 3, 'iron_dark'],
        [-2, 2, 'fire_orange'], [-1, 2, 'fire_yellow'], [0, 2, 'fire_orange'], [1, 2, 'fire_yellow'], [2, 2, 'fire_orange'],
        [-1, 1, 'fire_yellow'], [0, 1, 'fire_yellow'], [1, 1, 'fire_yellow'],
        [0, 0, 'fire_yellow'],  // Flame tip
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateTeslaTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-5, 10, 'stone'], [-4, 10, 'stone_dark'], [-3, 10, 'stone'], [-2, 10, 'stone_dark'],
        [-1, 10, 'stone'], [0, 10, 'stone_dark'], [1, 10, 'stone'], [2, 10, 'stone_dark'],
        [3, 10, 'stone'], [4, 10, 'stone_dark'], [5, 10, 'stone'],
        [-4, 11, 'stone_dark'], [-3, 11, 'stone'], [-2, 11, 'stone_dark'], [-1, 11, 'stone'],
        [0, 11, 'stone_dark'], [1, 11, 'stone'], [2, 11, 'stone_dark'], [3, 11, 'stone'], [4, 11, 'stone_dark'],
        // Iron spire
        [-2, 6, 'iron'], [-1, 6, 'iron_dark'], [0, 6, 'iron'], [1, 6, 'iron_dark'], [2, 6, 'iron'],
        [-2, 7, 'iron_dark'], [-1, 7, 'iron'], [0, 7, 'iron_dark'], [1, 7, 'iron'], [2, 7, 'iron_dark'],
        [-2, 8, 'iron'], [-1, 8, 'iron_dark'], [0, 8, 'iron'], [1, 8, 'iron_dark'], [2, 8, 'iron'],
        // Lightning orb
        [-2, 3, 'magic_blue'], [-1, 3, 'magic_light_blue'], [0, 3, 'magic_blue'], [1, 3, 'magic_light_blue'], [2, 3, 'magic_blue'],
        [-1, 2, 'magic_light_blue'], [0, 2, 'white'], [1, 2, 'magic_light_blue'],
        [0, 1, 'white'],  // Core
        // Small sparks
        [-3, 2, 'magic_light_blue'], [3, 2, 'magic_light_blue'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateTeslaTowerAttack(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-5, 10, 'stone'], [-4, 10, 'stone_dark'], [-3, 10, 'stone'], [-2, 10, 'stone_dark'],
        [-1, 10, 'stone'], [0, 10, 'stone_dark'], [1, 10, 'stone'], [2, 10, 'stone_dark'],
        [3, 10, 'stone'], [4, 10, 'stone_dark'], [5, 10, 'stone'],
        [-4, 11, 'stone_dark'], [-3, 11, 'stone'], [-2, 11, 'stone_dark'], [-1, 11, 'stone'],
        [0, 11, 'stone_dark'], [1, 11, 'stone'], [2, 11, 'stone_dark'], [3, 11, 'stone'], [4, 11, 'stone_dark'],
        // Iron spire
        [-2, 6, 'iron'], [-1, 6, 'iron_dark'], [0, 6, 'iron'], [1, 6, 'iron_dark'], [2, 6, 'iron'],
        [-2, 7, 'iron_dark'], [-1, 7, 'iron'], [0, 7, 'iron_dark'], [1, 7, 'iron'], [2, 7, 'iron_dark'],
        [-2, 8, 'iron'], [-1, 8, 'iron_dark'], [0, 8, 'iron'], [1, 8, 'iron_dark'], [2, 8, 'iron'],
        // Lightning orb (brighter)
        [-2, 3, 'magic_blue'], [-1, 3, 'white'], [0, 3, 'magic_blue'], [1, 3, 'white'], [2, 3, 'magic_blue'],
        [-1, 2, 'white'], [0, 2, 'white'], [1, 2, 'white'],
        // Lightning arcs
        [-4, 1, 'magic_light_blue'], [-3, 2, 'magic_light_blue'], [-2, 1, 'magic_light_blue'],
        [2, 1, 'magic_light_blue'], [3, 2, 'magic_light_blue'], [4, 1, 'magic_light_blue'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateBallistaTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Base - scaled for 48x48
        [-6, 10, 'wood'], [-5, 10, 'wood_dark'], [-4, 10, 'wood'], [-3, 10, 'wood_dark'],
        [-2, 10, 'wood'], [-1, 10, 'wood_dark'], [0, 10, 'wood'], [1, 10, 'wood_dark'],
        [2, 10, 'wood'], [3, 10, 'wood_dark'], [4, 10, 'wood'], [5, 10, 'wood_dark'], [6, 10, 'wood'],
        [-5, 11, 'wood_dark'], [-4, 11, 'wood'], [-3, 11, 'wood_dark'], [-2, 11, 'wood'],
        [-1, 11, 'wood_dark'], [0, 11, 'wood'], [1, 11, 'wood_dark'], [2, 11, 'wood'],
        [3, 11, 'wood_dark'], [4, 11, 'wood'], [5, 11, 'wood_dark'],
        // Ballista frame
        [-4, 6, 'wood'], [-3, 6, 'iron'], [-2, 6, 'wood'], [-1, 6, 'iron'], [0, 6, 'wood'],
        [1, 6, 'iron'], [2, 6, 'wood'], [3, 6, 'iron'], [4, 6, 'wood'],
        // Ballista arms
        [-4, 4, 'wood'], [-3, 4, 'iron'], [3, 4, 'iron'], [4, 4, 'wood'],
        // Bolt
        [-3, 3, 'iron'], [-2, 3, 'iron_dark'], [-1, 3, 'iron'], [0, 3, 'iron_dark'],
        [1, 3, 'iron'], [2, 3, 'iron_dark'], [3, 3, 'iron'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generatePoisonTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Wooden stand - scaled for 48x48
        [-5, 10, 'wood'], [-4, 10, 'wood_dark'], [-3, 10, 'wood'], [-2, 10, 'wood_dark'],
        [-1, 10, 'wood'], [0, 10, 'wood_dark'], [1, 10, 'wood'], [2, 10, 'wood_dark'],
        [3, 10, 'wood'], [4, 10, 'wood_dark'], [5, 10, 'wood'],
        [-4, 11, 'wood_dark'], [-3, 11, 'wood'], [-2, 11, 'wood_dark'], [-1, 11, 'wood'],
        [0, 11, 'wood_dark'], [1, 11, 'wood'], [2, 11, 'wood_dark'], [3, 11, 'wood'], [4, 11, 'wood_dark'],
        // Cauldron
        [-3, 6, 'iron_dark'], [-2, 6, 'iron'], [-1, 6, 'iron_dark'], [0, 6, 'iron'], [1, 6, 'iron_dark'],
        [2, 6, 'iron'], [3, 6, 'iron_dark'],
        [-3, 7, 'iron'], [-2, 7, 'poison_green'], [-1, 7, 'iron'], [0, 7, 'poison_green'], [1, 7, 'iron'],
        [2, 7, 'poison_green'], [3, 7, 'iron'],
        // Poison bubbles
        [-2, 4, 'poison_green'], [-1, 4, 'poison_dark_green'], [0, 4, 'poison_green'], [1, 4, 'poison_dark_green'],
        [2, 4, 'poison_green'],
        [-1, 3, 'poison_green'], [0, 3, 'poison_green'], [1, 3, 'poison_green'],  // Bubbles
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

function generateBombardTowerIdle(frame = 0) {
    const pixels = createPixelArray(TOWER_SIZE);
    const centerX = TOWER_SIZE / 2;
    const centerY = TOWER_SIZE / 2;
    const pixelList = [
        // Stone base - scaled for 48x48
        [-6, 10, 'stone'], [-5, 10, 'stone_dark'], [-4, 10, 'stone'], [-3, 10, 'stone_dark'],
        [-2, 10, 'stone'], [-1, 10, 'stone_dark'], [0, 10, 'stone'], [1, 10, 'stone_dark'],
        [2, 10, 'stone'], [3, 10, 'stone_dark'], [4, 10, 'stone'], [5, 10, 'stone_dark'], [6, 10, 'stone'],
        [-5, 11, 'stone_dark'], [-4, 11, 'stone'], [-3, 11, 'stone_dark'], [-2, 11, 'stone'],
        [-1, 11, 'stone_dark'], [0, 11, 'stone'], [1, 11, 'stone_dark'], [2, 11, 'stone'],
        [3, 11, 'stone_dark'], [4, 11, 'stone'], [5, 11, 'stone_dark'],
        // Cannon base
        [-3, 7, 'iron'], [-2, 7, 'iron_dark'], [-1, 7, 'iron'], [0, 7, 'iron_dark'], [1, 7, 'iron'],
        [2, 7, 'iron_dark'], [3, 7, 'iron'],
        // Cannon barrel
        [-3, 4, 'iron_dark'], [-2, 4, 'black'], [-1, 4, 'iron_dark'], [0, 4, 'black'], [1, 4, 'iron_dark'],
        [2, 4, 'black'], [3, 4, 'iron_dark'],
        [-2, 3, 'black'], [-1, 3, 'iron_dark'], [0, 3, 'black'], [1, 3, 'iron_dark'], [2, 3, 'black'],
        [-1, 2, 'black'], [0, 2, 'iron_dark'], [1, 2, 'black'],
        [0, 1, 'black'],  // Cannon mouth
    ];
    drawPixels(pixels, pixelList, centerX, centerY, TOWER_SIZE, false);
    return addOutline(pixels, TOWER_SIZE);
}

// ============================================================================
// Traps
// ============================================================================

function generateSpikeTrapIdle(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Ground
        [-4, 6, 'dirt'], [-3, 6, 'dirt_dark'], [-2, 6, 'dirt'], [-1, 6, 'dirt_dark'],
        [0, 6, 'dirt'], [1, 6, 'dirt_dark'], [2, 6, 'dirt'], [3, 6, 'dirt_dark'], [4, 6, 'dirt'],
        // Hidden spikes (small)
        [-1, 5, 'iron_dark'], [0, 5, 'iron'], [1, 5, 'iron_dark'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateSpikeTrapTrigger(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Ground
        [-4, 6, 'dirt'], [-3, 6, 'dirt_dark'], [-2, 6, 'dirt'], [-1, 6, 'dirt_dark'],
        [0, 6, 'dirt'], [1, 6, 'dirt_dark'], [2, 6, 'dirt'], [3, 6, 'dirt_dark'], [4, 6, 'dirt'],
        // Extended spikes
        [-1, 3, 'iron_dark'], [0, 3, 'iron'], [1, 3, 'iron_dark'],
        [-1, 4, 'iron'], [0, 4, 'iron'], [1, 4, 'iron'],
        [-1, 5, 'iron'], [0, 5, 'iron'], [1, 5, 'iron'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateFreezeTrapIdle(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Ground
        [-3, 6, 'dirt'], [-2, 6, 'dirt_dark'], [-1, 6, 'dirt'], [0, 6, 'dirt_dark'],
        [1, 6, 'dirt'], [2, 6, 'dirt_dark'], [3, 6, 'dirt'],
        // Frost crystal
        [0, 4, 'ice_blue'], [-1, 3, 'ice_light'], [0, 3, 'ice_white'], [1, 3, 'ice_light'],
        [0, 2, 'ice_blue'], [-1, 1, 'ice_light'], [0, 1, 'ice_white'], [1, 1, 'ice_light'],
        [0, 0, 'ice_blue'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateFirePitTrapIdle(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Ground
        [-3, 6, 'dirt'], [-2, 6, 'dirt_dark'], [-1, 6, 'dirt'], [0, 6, 'dirt_dark'],
        [1, 6, 'dirt'], [2, 6, 'dirt_dark'], [3, 6, 'dirt'],
        // Fire crack
        [-1, 4, 'fire_red'], [0, 4, 'fire_orange'], [1, 4, 'fire_red'],
        [0, 3, 'fire_yellow'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateMagicSnareTrapIdle(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Ground
        [-3, 6, 'dirt'], [-2, 6, 'dirt_dark'], [-1, 6, 'dirt'], [0, 6, 'dirt_dark'],
        [1, 6, 'dirt'], [2, 6, 'dirt_dark'], [3, 6, 'dirt'],
        // Rune circle
        [-2, 3, 'magic_purple'], [-1, 3, 'magic_light_purple'], [0, 3, 'magic_purple'],
        [1, 3, 'magic_light_purple'], [2, 3, 'magic_purple'],
        [-1, 2, 'magic_purple'], [0, 2, 'magic_light_purple'], [1, 2, 'magic_purple'],
        [0, 1, 'magic_purple'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateExplosiveBarrelIdle(frame = 0) {
    const pixels = createPixelArray();
    const pixelList = [
        // Barrel body
        [-2, 4, 'wood'], [-1, 4, 'wood_dark'], [0, 4, 'wood'], [1, 4, 'wood_dark'], [2, 4, 'wood'],
        [-2, 5, 'wood_dark'], [-1, 5, 'wood'], [0, 5, 'red'], [1, 5, 'wood'], [2, 5, 'wood_dark'],
        [-2, 6, 'wood'], [-1, 6, 'wood_dark'], [0, 6, 'wood'], [1, 6, 'wood_dark'], [2, 6, 'wood'],
        // Metal bands
        [-2, 4, 'iron_dark'], [2, 4, 'iron_dark'],
        [-2, 6, 'iron_dark'], [2, 6, 'iron_dark'],
        // Fuse
        [0, 3, 'fire_red'], [0, 2, 'fire_orange'], [0, 1, 'fire_yellow'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

// ============================================================================
// Enemies
// ============================================================================

function generateGruntRaiderIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -5, 'skin'], [-1, -4, 'skin'], [0, -4, 'skin_dark'], [1, -4, 'skin'],
        // Body - more detailed
        [-2, -2, 'leather'], [-1, -2, 'skin'], [0, -2, 'leather'], [1, -2, 'skin'], [2, -2, 'leather'],
        [-2, -1, 'leather'], [-1, -1, 'skin'], [0, -1, 'leather'], [1, -1, 'skin'], [2, -1, 'leather'],
        [-2, 0, 'leather'], [-1, 0, 'skin'], [0, 0, 'leather'], [1, 0, 'skin'], [2, 0, 'leather'],
        // Legs
        [-1, 3, 'leather'], [1, 3, 'leather'],
        [-1, 4, 'leather'], [1, 4, 'leather'],
        // Rusty axe - larger
        [0, 4, 'iron_dark'], [1, 4, 'wood'], [2, 4, 'wood'], [0, 5, 'iron_dark'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateGruntRaiderMove(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -5, 'skin'], [-1, -4, 'skin'], [0, -4, 'skin_dark'], [1, -4, 'skin'],
        // Body (slightly forward) - more detailed
        [-2, -2, 'leather'], [-1, -2, 'skin'], [0, -2, 'leather'], [1, -2, 'skin'], [2, -2, 'leather'],
        [-2, -1, 'leather'], [-1, -1, 'skin'], [0, -1, 'leather'], [1, -1, 'skin'], [2, -1, 'leather'],
        [-2, 0, 'leather'], [-1, 0, 'skin'], [0, 0, 'leather'], [1, 0, 'skin'], [2, 0, 'leather'],
        // Running legs
        [-2, 3, 'leather'], [2, 3, 'leather'],
        [-2, 4, 'leather'], [2, 4, 'leather'],
        // Axe held back
        [-1, 4, 'iron_dark'], [-2, 4, 'wood'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateGruntRaiderAttack(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -5, 'skin'], [-1, -4, 'skin'], [0, -4, 'skin_dark'], [1, -4, 'skin'],
        // Body - more detailed
        [-2, -2, 'leather'], [-1, -2, 'skin'], [0, -2, 'leather'], [1, -2, 'skin'], [2, -2, 'leather'],
        [-2, -1, 'leather'], [-1, -1, 'skin'], [0, -1, 'leather'], [1, -1, 'skin'], [2, -1, 'leather'],
        [-2, 0, 'leather'], [-1, 0, 'skin'], [0, 0, 'leather'], [1, 0, 'skin'], [2, 0, 'leather'],
        // Legs
        [-1, 3, 'leather'], [1, 3, 'leather'],
        [-1, 4, 'leather'], [1, 4, 'leather'],
        // Swinging axe - larger
        [-3, 2, 'iron_dark'], [-2, 2, 'wood'], [-1, 2, 'iron_dark'],
        [-3, 1, 'iron_dark'], [-2, 1, 'wood'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateBruteCrusherIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -6, 'orc_skin'], [-1, -5, 'orc_skin'], [0, -5, 'orc_skin_dark'], [1, -5, 'orc_skin'],
        [-1, -4, 'orc_skin'], [0, -4, 'orc_skin_dark'], [1, -4, 'orc_skin'],
        // Body - more detailed
        [-3, -2, 'orc_skin'], [-2, -2, 'orc_skin_dark'], [-1, -2, 'orc_skin'], [0, -2, 'orc_skin_dark'],
        [1, -2, 'orc_skin'], [2, -2, 'orc_skin_dark'], [3, -2, 'orc_skin'],
        [-3, -1, 'orc_skin_dark'], [-2, -1, 'orc_skin'], [-1, -1, 'orc_skin_dark'], [0, -1, 'orc_skin'],
        [1, -1, 'orc_skin_dark'], [2, -1, 'orc_skin'], [3, -1, 'orc_skin_dark'],
        [-3, 0, 'orc_skin'], [-2, 0, 'orc_skin_dark'], [-1, 0, 'orc_skin'], [0, 0, 'orc_skin_dark'],
        [1, 0, 'orc_skin'], [2, 0, 'orc_skin_dark'], [3, 0, 'orc_skin'],
        // Legs
        [-1, 3, 'orc_skin'], [1, 3, 'orc_skin'],
        [-1, 4, 'orc_skin'], [1, 4, 'orc_skin'],
        // Club - larger
        [0, 4, 'wood'], [0, 5, 'wood_dark'], [0, 6, 'wood'],
        [-1, 5, 'wood'], [1, 5, 'wood'],  // Club head
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateDirewolfIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -5, 'black'], [-1, -4, 'black'], [0, -4, 'shadow'], [1, -4, 'black'],
        [-1, -3, 'black'], [0, -3, 'shadow'], [1, -3, 'black'],
        // Body - more detailed
        [-3, -1, 'black'], [-2, -1, 'shadow'], [-1, -1, 'black'], [0, -1, 'shadow'],
        [1, -1, 'black'], [2, -1, 'shadow'], [3, -1, 'black'],
        [-3, 0, 'shadow'], [-2, 0, 'black'], [-1, 0, 'shadow'], [0, 0, 'black'],
        [1, 0, 'shadow'], [2, 0, 'black'], [3, 0, 'shadow'],
        // Legs
        [-2, 3, 'black'], [2, 3, 'black'],
        [-2, 4, 'black'], [2, 4, 'black'],
        // Tail
        [3, 0, 'black'], [3, -1, 'black'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateShieldbearerIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48
        [0, -5, 'skin'], [-1, -4, 'skin'], [0, -4, 'skin_dark'], [1, -4, 'skin'],
        // Shield (large) - more detailed
        [-4, -2, 'armor'], [-3, -2, 'armor_dark'], [-2, -2, 'armor'], [-1, -2, 'armor_dark'],
        [0, -2, 'armor'], [1, -2, 'armor_dark'],
        [-4, -1, 'armor_dark'], [-3, -1, 'armor'], [-2, -1, 'armor_dark'], [-1, -1, 'armor'],
        [0, -1, 'armor_dark'], [1, -1, 'armor'],
        [-4, 0, 'armor'], [-3, 0, 'armor_dark'], [-2, 0, 'armor'], [-1, 0, 'armor_dark'],
        [0, 0, 'armor'], [1, 0, 'armor_dark'],
        [-4, 1, 'armor_dark'], [-3, 1, 'armor'], [-2, 1, 'armor_dark'], [-1, 1, 'armor'],
        [0, 1, 'armor_dark'], [1, 1, 'armor'],
        // Body
        [0, 0, 'armor'], [1, 0, 'armor'],
        // Legs
        [-1, 3, 'armor'], [1, 3, 'armor'],
        [-1, 4, 'armor'], [1, 4, 'armor'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generatePyromancerIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Hood - larger for 48x48
        [0, -5, 'fire_red'], [-1, -4, 'fire_red'], [0, -4, 'fire_orange'], [1, -4, 'fire_red'],
        [-1, -3, 'fire_red'], [0, -3, 'fire_orange'], [1, -3, 'fire_red'],
        // Robes - more detailed
        [-2, -1, 'fire_red'], [-1, -1, 'fire_orange'], [0, -1, 'fire_red'], [1, -1, 'fire_orange'], [2, -1, 'fire_red'],
        [-2, 0, 'fire_orange'], [-1, 0, 'fire_red'], [0, 0, 'fire_orange'], [1, 0, 'fire_red'], [2, 0, 'fire_orange'],
        [-2, 1, 'fire_red'], [-1, 1, 'fire_orange'], [0, 1, 'fire_red'], [1, 1, 'fire_orange'], [2, 1, 'fire_red'],
        // Staff - larger
        [0, 3, 'wood'], [0, 4, 'wood'], [0, 5, 'fire_yellow'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateNecromancerIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Skull mask - larger for 48x48
        [0, -5, 'bone'], [-1, -4, 'black'], [0, -4, 'bone'], [1, -4, 'black'],
        [-1, -3, 'black'], [0, -3, 'bone'], [1, -3, 'black'],
        // Robes - more detailed
        [-2, -1, 'black'], [-1, -1, 'magic_purple'], [0, -1, 'black'], [1, -1, 'magic_purple'], [2, -1, 'black'],
        [-2, 0, 'magic_purple'], [-1, 0, 'black'], [0, 0, 'magic_purple'], [1, 0, 'black'], [2, 0, 'magic_purple'],
        [-2, 1, 'black'], [-1, 1, 'magic_purple'], [0, 1, 'black'], [1, 1, 'magic_purple'], [2, 1, 'black'],
        // Staff - larger
        [0, 3, 'bone'], [0, 4, 'magic_purple'], [0, 5, 'magic_light_purple'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateSkeletonMinionIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Skull - larger for 48x48
        [0, -4, 'bone'], [-1, -3, 'bone'], [0, -3, 'bone_dark'], [1, -3, 'bone'],
        // Body - more detailed
        [-1, -1, 'bone_dark'], [0, -1, 'bone'], [1, -1, 'bone_dark'],
        [-1, 0, 'bone'], [0, 0, 'bone_dark'], [1, 0, 'bone'],
        [-1, 1, 'bone_dark'], [0, 1, 'bone'], [1, 1, 'bone_dark'],
        // Legs
        [-1, 3, 'bone'], [1, 3, 'bone'],
        [-1, 4, 'bone'], [1, 4, 'bone'],
        // Dagger - larger
        [0, 4, 'iron_dark'], [0, 5, 'iron'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateBoulderRamCrewIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Left soldier - larger for 48x48
        [-4, -2, 'skin'], [-4, -1, 'skin'], [-4, 0, 'leather'], [-4, 1, 'leather'],
        [-4, 2, 'leather'],
        // Right soldier
        [4, -2, 'skin'], [4, -1, 'skin'], [4, 0, 'leather'], [4, 1, 'leather'],
        [4, 2, 'leather'],
        // Ram - larger and more detailed
        [-3, 0, 'wood'], [-2, 0, 'iron_dark'], [-1, 0, 'wood'], [0, 0, 'iron_dark'],
        [1, 0, 'wood'], [2, 0, 'iron_dark'], [3, 0, 'wood'],
        [-2, 1, 'wood'], [-1, 1, 'iron_dark'], [0, 1, 'wood'], [1, 1, 'iron_dark'], [2, 1, 'wood'],
        [-1, 2, 'wood'], [0, 2, 'wood'], [1, 2, 'wood'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

// ============================================================================
// Bosses
// ============================================================================

function generateIronbackMinotaurIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48 boss
        [0, -7, 'orc_skin_dark'], [-1, -6, 'orc_skin'], [0, -6, 'red'], [1, -6, 'orc_skin'],
        [-1, -5, 'orc_skin'], [0, -5, 'red'], [1, -5, 'orc_skin'],
        // Horns
        [-2, -5, 'orc_skin_dark'], [2, -5, 'orc_skin_dark'],
        [-3, -4, 'orc_skin_dark'], [3, -4, 'orc_skin_dark'],
        // Armored body - more detailed
        [-4, -3, 'armor'], [-3, -3, 'armor_dark'], [-2, -3, 'armor'], [-1, -3, 'armor_dark'],
        [0, -3, 'red'], [1, -3, 'armor_dark'], [2, -3, 'armor'], [3, -3, 'armor_dark'], [4, -3, 'armor'],
        [-4, -2, 'armor_dark'], [-3, -2, 'armor'], [-2, -2, 'armor_dark'], [-1, -2, 'armor'],
        [0, -2, 'red'], [1, -2, 'armor'], [2, -2, 'armor_dark'], [3, -2, 'armor'], [4, -2, 'armor_dark'],
        [-4, -1, 'armor'], [-3, -1, 'armor_dark'], [-2, -1, 'armor'], [-1, -1, 'armor_dark'],
        [0, -1, 'red'], [1, -1, 'armor_dark'], [2, -1, 'armor'], [3, -1, 'armor_dark'], [4, -1, 'armor'],
        [-4, 0, 'armor_dark'], [-3, 0, 'armor'], [-2, 0, 'armor_dark'], [-1, 0, 'armor'],
        [0, 0, 'red'], [1, 0, 'armor'], [2, 0, 'armor_dark'], [3, 0, 'armor'], [4, 0, 'armor_dark'],
        // Legs
        [-1, 3, 'armor'], [1, 3, 'armor'],
        [-1, 4, 'armor'], [1, 4, 'armor'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateFireDrakeIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Head - larger for 48x48 boss
        [0, -6, 'fire_orange'], [-1, -5, 'fire_red'], [0, -5, 'fire_yellow'], [1, -5, 'fire_red'],
        [-1, -4, 'fire_red'], [0, -4, 'fire_yellow'], [1, -4, 'fire_red'],
        // Body - more detailed
        [-3, -2, 'fire_red'], [-2, -2, 'fire_orange'], [-1, -2, 'fire_yellow'], [0, -2, 'fire_red'],
        [1, -2, 'fire_yellow'], [2, -2, 'fire_orange'], [3, -2, 'fire_red'],
        [-3, -1, 'fire_orange'], [-2, -1, 'fire_red'], [-1, -1, 'fire_yellow'], [0, -1, 'fire_orange'],
        [1, -1, 'fire_yellow'], [2, -1, 'fire_red'], [3, -1, 'fire_orange'],
        [-3, 0, 'fire_red'], [-2, 0, 'fire_orange'], [-1, 0, 'fire_yellow'], [0, 0, 'fire_red'],
        [1, 0, 'fire_yellow'], [2, 0, 'fire_orange'], [3, 0, 'fire_red'],
        // Legs
        [-1, 3, 'fire_red'], [1, 3, 'fire_red'],
        [-1, 4, 'fire_red'], [1, 4, 'fire_red'],
        // Tail
        [3, 1, 'fire_orange'], [3, 0, 'fire_red'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

function generateLichKingIdle(frame = 0) {
    const pixels = createPixelArray(ENEMY_SIZE);
    const centerX = ENEMY_SIZE / 2;
    const centerY = ENEMY_SIZE / 2;
    const pixelList = [
        // Crown/head - larger for 48x48 boss
        [0, -6, 'ice_blue'], [-1, -5, 'ice_light'], [0, -5, 'ice_white'], [1, -5, 'ice_light'],
        [-1, -4, 'ice_light'], [0, -4, 'ice_white'], [1, -4, 'ice_light'],
        // Robes - more detailed
        [-3, -2, 'black'], [-2, -2, 'ice_blue'], [-1, -2, 'ice_light'], [0, -2, 'ice_white'],
        [1, -2, 'ice_light'], [2, -2, 'ice_blue'], [3, -2, 'black'],
        [-3, -1, 'black'], [-2, -1, 'ice_blue'], [-1, -1, 'ice_light'], [0, -1, 'ice_white'],
        [1, -1, 'ice_light'], [2, -1, 'ice_blue'], [3, -1, 'black'],
        [-3, 0, 'black'], [-2, 0, 'ice_blue'], [-1, 0, 'ice_light'], [0, 0, 'ice_white'],
        [1, 0, 'ice_light'], [2, 0, 'ice_blue'], [3, 0, 'black'],
        [-3, 1, 'black'], [-2, 1, 'ice_blue'], [-1, 1, 'ice_light'], [0, 1, 'ice_white'],
        [1, 1, 'ice_light'], [2, 1, 'ice_blue'], [3, 1, 'black'],
        // Staff - larger
        [0, 3, 'ice_blue'], [0, 4, 'ice_white'], [0, 5, 'ice_light'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, ENEMY_SIZE, false);
    return addOutline(pixels, ENEMY_SIZE);
}

// ============================================================================
// Projectiles
// ============================================================================

function generateArrowProjectile(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger arrow for 24x24 - more visible
        [0, -2, 'wood'], [0, -1, 'wood_dark'], [0, 0, 'wood'], [0, 1, 'wood_dark'],
        [0, 2, 'iron'], [0, 3, 'iron_dark'],  // Arrowhead
        [-1, 2, 'iron'], [1, 2, 'iron'],  // Wider arrowhead
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generateBoltProjectile(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger bolt for 24x24 - more visible
        [0, -2, 'iron'], [0, -1, 'iron_dark'], [0, 0, 'iron'], [0, 1, 'iron_dark'],
        [0, 2, 'iron'], [0, 3, 'iron_dark'],  // Bolt tip
        [-1, 2, 'iron'], [1, 2, 'iron'],  // Wider tip
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generateRockProjectile(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger rock for 24x24 - more visible
        [-2, -2, 'stone'], [-1, -2, 'stone_dark'], [0, -2, 'stone'], [1, -2, 'stone_dark'], [2, -2, 'stone'],
        [-2, -1, 'stone_dark'], [-1, -1, 'stone'], [0, -1, 'stone_dark'], [1, -1, 'stone'], [2, -1, 'stone_dark'],
        [-2, 0, 'stone'], [-1, 0, 'stone_dark'], [0, 0, 'stone'], [1, 0, 'stone_dark'], [2, 0, 'stone'],
        [-2, 1, 'stone_dark'], [-1, 1, 'stone'], [0, 1, 'stone_dark'], [1, 1, 'stone'], [2, 1, 'stone_dark'],
        [-2, 2, 'stone'], [-1, 2, 'stone_dark'], [0, 2, 'stone'], [1, 2, 'stone_dark'], [2, 2, 'stone'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generateFireballProjectile(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger fireball for 24x24
        [-2, 0, 'fire_orange'], [-1, 0, 'fire_yellow'], [0, 0, 'fire_red'], [1, 0, 'fire_yellow'], [2, 0, 'fire_orange'],
        [0, -1, 'fire_red'], [0, 1, 'fire_red'],
        [-1, -1, 'fire_orange'], [1, -1, 'fire_orange'],
        [-1, 1, 'fire_orange'], [1, 1, 'fire_orange'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generateLightningArc(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger lightning for 24x24 - more visible
        [-2, 0, 'magic_light_blue'], [-1, 0, 'white'], [0, 0, 'magic_blue'], [1, 0, 'white'], [2, 0, 'magic_light_blue'],
        [0, -1, 'magic_blue'], [0, 1, 'magic_blue'],
        [-1, -1, 'magic_light_blue'], [1, -1, 'magic_light_blue'],
        [-1, 1, 'magic_light_blue'], [1, 1, 'magic_light_blue'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generatePoisonGlob(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger poison glob for 24x24 - more visible
        [-1, -1, 'poison_green'], [0, -1, 'poison_dark_green'], [1, -1, 'poison_green'],
        [-1, 0, 'poison_dark_green'], [0, 0, 'poison_green'], [1, 0, 'poison_dark_green'],
        [-1, 1, 'poison_green'], [0, 1, 'poison_dark_green'], [1, 1, 'poison_green'],
        [0, 2, 'poison_green'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

function generateCannonball(frame = 0) {
    const pixels = createPixelArray(PROJECTILE_SIZE);
    const centerX = PROJECTILE_SIZE / 2;
    const centerY = PROJECTILE_SIZE / 2;
    const pixelList = [
        // Larger cannonball for 24x24 - more visible
        [-2, -2, 'iron_dark'], [-1, -2, 'iron'], [0, -2, 'iron_dark'], [1, -2, 'iron'], [2, -2, 'iron_dark'],
        [-2, -1, 'iron'], [-1, -1, 'iron_dark'], [0, -1, 'iron'], [1, -1, 'iron_dark'], [2, -1, 'iron'],
        [-2, 0, 'iron_dark'], [-1, 0, 'iron'], [0, 0, 'iron_dark'], [1, 0, 'iron'], [2, 0, 'iron_dark'],
        [-2, 1, 'iron'], [-1, 1, 'iron_dark'], [0, 1, 'iron'], [1, 1, 'iron_dark'], [2, 1, 'iron'],
        [-2, 2, 'iron_dark'], [-1, 2, 'iron'], [0, 2, 'iron_dark'], [1, 2, 'iron'], [2, 2, 'iron_dark'],
    ];
    drawPixels(pixels, pixelList, centerX, centerY, PROJECTILE_SIZE, false);
    return addProjectileOutline(pixels, PROJECTILE_SIZE);
}

// ============================================================================
// UI Icons
// ============================================================================

function generateUICoin() {
    const pixels = createPixelArray();
    const pixelList = [
        [-1, -1, 'gold'], [0, -1, 'gold'], [1, -1, 'gold'],
        [-1, 0, 'gold'], [0, 0, 'fire_yellow'], [1, 0, 'gold'],
        [-1, 1, 'gold'], [0, 1, 'gold'], [1, 1, 'gold'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUIHeart() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -2, 'red'], [-1, -1, 'red'], [0, -1, 'red'], [1, -1, 'red'],
        [-1, 0, 'red'], [0, 0, 'red'], [1, 0, 'red'],
        [0, 1, 'red'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUIShield() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -2, 'armor'],
        [-1, -1, 'armor'], [0, -1, 'blue'], [1, -1, 'armor'],
        [-1, 0, 'armor'], [0, 0, 'blue'], [1, 0, 'armor'],
        [-1, 1, 'armor'], [0, 1, 'blue'], [1, 1, 'armor'],
        [0, 2, 'armor'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUISword() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -2, 'iron'], [0, -1, 'iron'], [0, 0, 'iron'], [0, 1, 'iron'],
        [-1, 1, 'iron_dark'], [1, 1, 'iron_dark'],  // Cross guard
        [0, 2, 'wood'],  // Hilt
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUIFire() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -1, 'fire_yellow'], [0, 0, 'fire_orange'], [0, 1, 'fire_red'],
        [-1, 0, 'fire_orange'], [1, 0, 'fire_orange'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUILightning() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -1, 'magic_light_blue'], [0, 0, 'white'], [0, 1, 'magic_blue'],
        [-1, 0, 'magic_light_blue'], [1, 0, 'magic_light_blue'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUIPoison() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, 0, 'poison_green'], [0, 1, 'poison_dark_green'],
        [-1, 1, 'poison_green'], [1, 1, 'poison_green'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

function generateUISkull() {
    const pixels = createPixelArray();
    const pixelList = [
        [0, -2, 'bone'],
        [-1, -1, 'bone'], [0, -1, 'bone'], [1, -1, 'bone'],
        [-1, 0, 'bone'], [0, 0, 'black'], [1, 0, 'bone'],  // Eye sockets
        [-1, 1, 'bone'], [0, 1, 'bone'], [1, 1, 'bone'],
    ];
    drawPixels(pixels, pixelList);
    return pixels;
}

// ============================================================================
// Main Generation
// ============================================================================

async function generateAllAssets() {
    console.log("Generating pixel art assets...\n");
    
    const baseDir = path.join(__dirname, 'assets', 'images');
    
    // Environment tiles
    const envPath = path.join(baseDir, 'environment');
    await saveSprite(generateCastleWallHorizontal(), path.join(envPath, 'castle_wall_straight_horizontal.png'));
    await saveSprite(generateCastleWallVertical(), path.join(envPath, 'castle_wall_straight_vertical.png'));
    await saveSprite(generateCastleWallCornerNE(), path.join(envPath, 'castle_wall_corner_NE.png'));
    await saveSprite(generateCastleWallCornerNW(), path.join(envPath, 'castle_wall_corner_NW.png'));
    await saveSprite(generateCastleWallCornerSE(), path.join(envPath, 'castle_wall_corner_SE.png'));
    await saveSprite(generateCastleWallCornerSW(), path.join(envPath, 'castle_wall_corner_SW.png'));
    await saveSprite(generateCastleGateClosed(), path.join(envPath, 'castle_gate_closed.png'));
    await saveSprite(generateCastleGateOpenTop(), path.join(envPath, 'castle_gate_open_top.png'));
    await saveSprite(generateCastleGateOpenBottom(), path.join(envPath, 'castle_gate_open_bottom.png'));
    await saveSprite(generateCastleFloorStone(), path.join(envPath, 'castle_floor_stone.png'));
    
    for (const variant of ['A', 'B', 'C']) {
        await saveSprite(generateGrassTile(variant), path.join(envPath, `grass_tile_${variant}.png`));
        await saveSprite(generatePathTile(variant), path.join(envPath, `path_tile_${variant}.png`));
    }
    
    // Towers
    const towers = {
        'arrow_tower': { idle: generateArrowTowerIdle, attack: generateArrowTowerAttack, death: generateArrowTowerDeath },
        'catapult_tower': { idle: generateCatapultTowerIdle, attack: generateCatapultTowerAttack, death: generateCatapultTowerIdle },
        'crossbow_tower': { idle: generateCrossbowTowerIdle, attack: generateCrossbowTowerIdle, death: generateCrossbowTowerIdle },
        'fire_tower': { idle: generateFireTowerIdle, attack: generateFireTowerAttack, death: generateFireTowerIdle },
        'tesla_tower': { idle: generateTeslaTowerIdle, attack: generateTeslaTowerAttack, death: generateTeslaTowerIdle },
        'ballista_tower': { idle: generateBallistaTowerIdle, attack: generateBallistaTowerIdle, death: generateBallistaTowerIdle },
        'poison_tower': { idle: generatePoisonTowerIdle, attack: generatePoisonTowerIdle, death: generatePoisonTowerIdle },
        'bombard_tower': { idle: generateBombardTowerIdle, attack: generateBombardTowerIdle, death: generateBombardTowerIdle },
    };
    
    for (const [name, anims] of Object.entries(towers)) {
        const towerPath = path.join(baseDir, 'towers', name);
        for (let frame = 0; frame < 3; frame++) {
            await saveSprite(anims.idle(frame), path.join(towerPath, 'idle', `${frame}.png`), TOWER_SIZE);
        }
        for (let frame = 0; frame < 5; frame++) {
            await saveSprite(anims.attack(frame), path.join(towerPath, 'attack', `${frame}.png`), TOWER_SIZE);
        }
        for (let frame = 0; frame < 4; frame++) {
            const deathSprite = anims.death(frame);
            // Ensure sprite is correct size
            const spriteSize = deathSprite.length === TOWER_SIZE * TOWER_SIZE * 4 ? TOWER_SIZE : 32;
            await saveSprite(deathSprite, path.join(towerPath, 'death', `${frame}.png`), spriteSize);
        }
    }
    
    // Traps
    const traps = {
        'spike_trap': { idle: generateSpikeTrapIdle, trigger: generateSpikeTrapTrigger },
        'freeze_trap': { idle: generateFreezeTrapIdle, trigger: generateFreezeTrapIdle },
        'fire_pit_trap': { idle: generateFirePitTrapIdle, trigger: generateFirePitTrapIdle },
        'magic_snare_trap': { idle: generateMagicSnareTrapIdle, trigger: generateMagicSnareTrapIdle },
        'explosive_barrel': { idle: generateExplosiveBarrelIdle, trigger: generateExplosiveBarrelIdle },
    };
    
    for (const [name, anims] of Object.entries(traps)) {
        const trapPath = path.join(baseDir, 'traps', name);
        for (let frame = 0; frame < 2; frame++) {
            await saveSprite(anims.idle(frame), path.join(trapPath, 'idle', `${frame}.png`));
        }
        for (let frame = 0; frame < 4; frame++) {
            await saveSprite(anims.trigger(frame), path.join(trapPath, 'trigger', `${frame}.png`));
        }
        for (let frame = 0; frame < 3; frame++) {
            await saveSprite(anims.idle(frame), path.join(trapPath, 'destroy', `${frame}.png`));
        }
    }
    
    // Enemies
    const enemies = {
        'grunt_raider': { idle: generateGruntRaiderIdle, move: generateGruntRaiderMove, attack: generateGruntRaiderAttack },
        'brute_crusher': { idle: generateBruteCrusherIdle, move: generateBruteCrusherIdle, attack: generateBruteCrusherIdle },
        'direwolf': { idle: generateDirewolfIdle, move: generateDirewolfIdle, attack: generateDirewolfIdle },
        'shieldbearer': { idle: generateShieldbearerIdle, move: generateShieldbearerIdle, attack: generateShieldbearerIdle },
        'pyromancer': { idle: generatePyromancerIdle, move: generatePyromancerIdle, attack: generatePyromancerIdle },
        'necromancer': { idle: generateNecromancerIdle, move: generateNecromancerIdle, attack: generateNecromancerIdle },
        'skeleton_minion': { idle: generateSkeletonMinionIdle, move: generateSkeletonMinionIdle, attack: generateSkeletonMinionIdle },
        'boulder_ram_crew': { idle: generateBoulderRamCrewIdle, move: generateBoulderRamCrewIdle, attack: generateBoulderRamCrewIdle },
    };
    
    for (const [name, anims] of Object.entries(enemies)) {
        const enemyPath = path.join(baseDir, 'enemies', name);
        for (let frame = 0; frame < 3; frame++) {
            const sprite = anims.idle(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(enemyPath, 'idle', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 6; frame++) {
            const sprite = anims.move(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(enemyPath, 'move', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 5; frame++) {
            const sprite = anims.attack(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(enemyPath, 'attack', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 5; frame++) {
            const sprite = anims.idle(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(enemyPath, 'death', `${frame}.png`), size);
        }
    }
    
    // Bosses
    const bosses = {
        'ironback_minotaur': { idle: generateIronbackMinotaurIdle, move: generateIronbackMinotaurIdle, attack: generateIronbackMinotaurIdle },
        'fire_drake': { idle: generateFireDrakeIdle, move: generateFireDrakeIdle, attack: generateFireDrakeIdle },
        'lich_king_arcthros': { idle: generateLichKingIdle, move: generateLichKingIdle, attack: generateLichKingIdle },
    };
    
    for (const [name, anims] of Object.entries(bosses)) {
        const bossPath = path.join(baseDir, 'bosses', name);
        for (let frame = 0; frame < 3; frame++) {
            const sprite = anims.idle(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(bossPath, 'idle', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 6; frame++) {
            const sprite = anims.move(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(bossPath, 'move', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 6; frame++) {
            const sprite = anims.attack(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(bossPath, 'attack', `${frame}.png`), size);
        }
        for (let frame = 0; frame < 6; frame++) {
            const sprite = anims.idle(frame);
            const size = sprite.length === ENEMY_SIZE * ENEMY_SIZE * 4 ? ENEMY_SIZE : 32;
            await saveSprite(sprite, path.join(bossPath, 'death', `${frame}.png`), size);
        }
    }
    
    // Projectiles
    const projectiles = {
        'arrow_projectile': generateArrowProjectile,
        'bolt_projectile': generateBoltProjectile,
        'rock_projectile': generateRockProjectile,
        'fireball_projectile': generateFireballProjectile,
        'lightning_arc': generateLightningArc,
        'poison_glob': generatePoisonGlob,
        'cannonball': generateCannonball,
    };
    
    for (const [name, genFn] of Object.entries(projectiles)) {
        const projPath = path.join(baseDir, 'projectiles', name);
        for (let frame = 0; frame < 3; frame++) {
            const sprite = genFn(frame);
            const size = sprite.length === PROJECTILE_SIZE * PROJECTILE_SIZE * 4 ? PROJECTILE_SIZE : 32;
            await saveSprite(sprite, path.join(projPath, 'flying', `${frame}.png`), size);
        }
    }
    
    // UI Icons
    const uiPath = path.join(baseDir, 'ui');
    await saveSprite(generateUICoin(), path.join(uiPath, 'ui_coin.png'));
    await saveSprite(generateUIHeart(), path.join(uiPath, 'ui_heart.png'));
    await saveSprite(generateUIShield(), path.join(uiPath, 'ui_shield.png'));
    await saveSprite(generateUISword(), path.join(uiPath, 'ui_sword.png'));
    await saveSprite(generateUIFire(), path.join(uiPath, 'ui_fire.png'));
    await saveSprite(generateUILightning(), path.join(uiPath, 'ui_lightning.png'));
    await saveSprite(generateUIPoison(), path.join(uiPath, 'ui_poison.png'));
    await saveSprite(generateUISkull(), path.join(uiPath, 'ui_skull.png'));
    
    console.log("\n✅ All pixel art assets generated successfully!");
}

// Run generation
generateAllAssets().catch(console.error);

