#!/usr/bin/env node

/**
 * Optimize existing sprite images - resize and compress for better performance
 */

import fs from 'fs';
import path from 'path';
import sharp from 'sharp';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const assetsBaseDir = path.join(__dirname, 'assets', 'images');

/**
 * Get optimal sprite dimensions based on category
 */
function getSpriteDimensions(category) {
  const dimensions = {
    enemies: { targetWidth: 48, targetHeight: 48 },
    towers: { targetWidth: 96, targetHeight: 96 },
    traps: { targetWidth: 48, targetHeight: 48 },
    bosses: { targetWidth: 128, targetHeight: 128 },
    environment: { targetWidth: 96, targetHeight: 96 },
    projectiles: { targetWidth: 24, targetHeight: 24 }
  };
  
  return dimensions[category] || { targetWidth: 48, targetHeight: 48 };
}

/**
 * Optimize a single sprite file
 */
async function optimizeSprite(filePath, category) {
  try {
    const dims = getSpriteDimensions(category);
    const tempPath = filePath + '.tmp';
    
    // Read, resize, and optimize
    await sharp(filePath)
      .resize(dims.targetWidth, dims.targetHeight, {
        fit: 'contain',
        withoutEnlargement: false,
        background: { r: 0, g: 0, b: 0, alpha: 0 }
      })
      .png({ 
        compressionLevel: 9, 
        quality: 90,
        effort: 7,
        palette: true
      })
      .toFile(tempPath);
    
    // Replace original with optimized version
    fs.renameSync(tempPath, filePath);
    
    const stats = fs.statSync(filePath);
    return stats.size;
  } catch (error) {
    console.error(`❌ Error optimizing ${filePath}:`, error.message);
    return null;
  }
}

/**
 * Process all sprites in a directory
 */
async function processDirectory(dirPath, category) {
  const files = fs.readdirSync(dirPath, { withFileTypes: true });
  let optimized = 0;
  let totalSize = 0;
  
  for (const file of files) {
    const fullPath = path.join(dirPath, file.name);
    
    if (file.isDirectory()) {
      const result = await processDirectory(fullPath, category);
      optimized += result.optimized;
      totalSize += result.totalSize;
    } else if (file.name.endsWith('.png')) {
      const size = await optimizeSprite(fullPath, category);
      if (size !== null) {
        optimized++;
        totalSize += size;
        console.log(`✅ Optimized: ${path.relative(assetsBaseDir, fullPath)} (${(size / 1024).toFixed(2)} KB)`);
      }
    }
  }
  
  return { optimized, totalSize };
}

/**
 * Main optimization function
 */
async function optimizeAllSprites() {
  console.log('🔧 Optimizing existing sprites...\n');
  
  const categories = ['towers', 'traps', 'enemies', 'bosses', 'environment', 'projectiles'];
  let totalOptimized = 0;
  let totalSize = 0;
  
  for (const category of categories) {
    const categoryPath = path.join(assetsBaseDir, category);
    if (fs.existsSync(categoryPath)) {
      console.log(`📦 Processing ${category}...`);
      const result = await processDirectory(categoryPath, category);
      totalOptimized += result.optimized;
      totalSize += result.totalSize;
      console.log(`   Optimized ${result.optimized} files\n`);
    }
  }
  
  console.log('='.repeat(60));
  console.log('✨ Optimization Complete!');
  console.log(`   Total files optimized: ${totalOptimized}`);
  console.log(`   Total size: ${(totalSize / 1024 / 1024).toFixed(2)} MB`);
  console.log('='.repeat(60));
}

// Run optimization
optimizeAllSprites().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});

