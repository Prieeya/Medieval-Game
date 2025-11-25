#!/usr/bin/env node

/**
 * Main sprite generation script for Medieval Siege Simulator
 * Generates all animated sprites using Stability AI (Stable Diffusion)
 */

import 'dotenv/config';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import sharp from 'sharp';
import axios from 'axios';
import assetsConfig from './assetsConfigAnimated.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const STABILITY_API_KEY = process.env.STABILITY_API_KEY;
const STABILITY_API_URL = 'https://api.stability.ai/v1/generation/stable-diffusion-xl-1024-v1-0/text-to-image';

if (!STABILITY_API_KEY) {
  console.error('❌ ERROR: STABILITY_API_KEY environment variable not set!');
  console.error('   Please set it in .env file: STABILITY_API_KEY="your-api-key"');
  console.error('   Get your free API key at: https://platform.stability.ai/');
  process.exit(1);
}

// Base directory for assets
const assetsBaseDir = path.join(__dirname, 'assets', 'images');

/**
 * Create directory structure if it doesn't exist
 */
function ensureDirectoryExists(dirPath) {
  if (!fs.existsSync(dirPath)) {
    fs.mkdirSync(dirPath, { recursive: true });
    console.log(`📁 Created directory: ${dirPath}`);
  }
}

/**
 * Generate a single sprite frame using Stability AI API
 */
async function generateSpriteFrame(category, name, animation, frameIndex, totalFrames, prompt) {
  const frameNumber = frameIndex + 1;
  const outputPath = path.join(assetsBaseDir, category, name, animation, `frame_${frameNumber.toString().padStart(2, '0')}.png`);
  
  // Skip if already exists
  if (fs.existsSync(outputPath)) {
    console.log(`⏭️  Skipping ${category}/${name}/${animation}/frame_${frameNumber} (already exists)`);
    return true;
  }

  try {
    console.log(`🎨 Generating ${category}/${name}/${animation}/frame_${frameNumber}/${totalFrames}...`);
    
    // Enhanced prompt with frame information
    const enhancedPrompt = `${prompt}, frame ${frameNumber} of ${totalFrames} animation sequence`;
    
    // Negative prompt to avoid unwanted elements
    const negativePrompt = "3D, perspective, isometric, low quality, blurry, distorted, watermark, text, signature";
    
    // Call Stability AI API
    const response = await axios.post(
      STABILITY_API_URL,
      {
        text_prompts: [
          {
            text: enhancedPrompt,
            weight: 1.0
          },
          {
            text: negativePrompt,
            weight: -1.0
          }
        ],
        cfg_scale: 7,
        height: 1024,
        width: 1024,
        samples: 1,
        steps: 30,
        style_preset: "enhance" // Helps with quality
      },
      {
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'Authorization': `Bearer ${STABILITY_API_KEY}`
        }
      }
    );

    if (!response.data || !response.data.artifacts || response.data.artifacts.length === 0) {
      throw new Error('Invalid response from Stability AI API');
    }

    // Stability AI returns base64 encoded images in artifacts array
    const artifact = response.data.artifacts[0];
    const base64Image = artifact.base64;
    
    if (!base64Image) {
      throw new Error('No image data in response');
    }

    // Convert base64 to buffer
    const imageBuffer = Buffer.from(base64Image, 'base64');
    
    // Process with sharp: trim transparent borders and save
    await sharp(imageBuffer)
      .trim({ threshold: 10 })
      .png()
      .toFile(outputPath);
    
    console.log(`✅ Generated ${category}/${name}/${animation}/frame_${frameNumber}.png`);
    
    // Rate limiting: wait 2 seconds between requests to avoid API limits
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    return true;
  } catch (error) {
    if (error.response) {
      console.error(`❌ API Error (${error.response.status}):`, error.response.data?.message || error.message);
      if (error.response.status === 401) {
        console.error('   → Check if your API key is valid');
      } else if (error.response.status === 429) {
        console.error('   → Rate limit exceeded, waiting longer...');
        await new Promise(resolve => setTimeout(resolve, 5000));
        return false; // Retry on next run
      }
    } else {
      console.error(`❌ Error generating ${category}/${name}/${animation}/frame_${frameNumber}:`, error.message);
    }
    return false;
  }
}

/**
 * Generate all sprites for a single asset
 */
async function generateAssetSprites(asset) {
  const { category, name, animations } = asset;
  
  console.log(`\n${'='.repeat(60)}`);
  console.log(`📦 Processing: ${category}/${name}`);
  console.log(`${'='.repeat(60)}`);
  
  // Create base directory for this asset
  const assetDir = path.join(assetsBaseDir, category, name);
  ensureDirectoryExists(assetDir);
  
  let totalFrames = 0;
  let generatedFrames = 0;
  
  // Generate all animations
  for (const [animationName, animationConfig] of Object.entries(animations)) {
    const { frames, prompt } = animationConfig;
    
    // Create animation directory
    const animationDir = path.join(assetDir, animationName);
    ensureDirectoryExists(animationDir);
    
    // Generate each frame
    for (let frameIndex = 0; frameIndex < frames; frameIndex++) {
      totalFrames++;
      const success = await generateSpriteFrame(
        category,
        name,
        animationName,
        frameIndex,
        frames,
        prompt
      );
      if (success) {
        generatedFrames++;
      }
    }
  }
  
  console.log(`\n✅ Completed ${category}/${name}: ${generatedFrames}/${totalFrames} frames generated\n`);
  return { totalFrames, generatedFrames };
}

/**
 * Main generation function
 */
async function generateAllSprites() {
  console.log('🚀 Starting sprite generation pipeline...\n');
  console.log(`📂 Output directory: ${assetsBaseDir}\n`);
  console.log(`🤖 Using: Stability AI (Stable Diffusion XL)\n`);
  
  // Create base directory structure
  const categories = ['towers', 'traps', 'enemies', 'bosses', 'environment', 'projectiles'];
  categories.forEach(cat => {
    ensureDirectoryExists(path.join(assetsBaseDir, cat));
  });
  
  let totalAssets = 0;
  let totalFrames = 0;
  let totalGenerated = 0;
  
  // Process each asset
  for (const asset of assetsConfig) {
    totalAssets++;
    const result = await generateAssetSprites(asset);
    totalFrames += result.totalFrames;
    totalGenerated += result.generatedFrames;
  }
  
  // Summary
  console.log('\n' + '='.repeat(60));
  console.log('📊 GENERATION SUMMARY');
  console.log('='.repeat(60));
  console.log(`Total Assets: ${totalAssets}`);
  console.log(`Total Frames: ${totalFrames}`);
  console.log(`Generated: ${totalGenerated}`);
  console.log(`Skipped: ${totalFrames - totalGenerated}`);
  console.log('='.repeat(60));
  console.log('\n✨ Sprite generation complete!\n');
}

// Run the generator
generateAllSprites().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
