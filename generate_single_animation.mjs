#!/usr/bin/env node

/**
 * Single animation generator script
 * Usage: node generate_single_animation.mjs <category> <name> <animation>
 * Example: node generate_single_animation.mjs towers arrow_tower attack
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

// Parse command line arguments
const args = process.argv.slice(2);
if (args.length !== 3) {
  console.error('❌ Usage: node generate_single_animation.mjs <category> <name> <animation>');
  console.error('   Example: node generate_single_animation.mjs towers arrow_tower attack');
  process.exit(1);
}

const [category, name, animation] = args;

// Find the asset in config
const asset = assetsConfig.find(
  a => a.category === category && a.name === name
);

if (!asset) {
  console.error(`❌ Asset not found: ${category}/${name}`);
  console.error('   Available assets:');
  assetsConfig.forEach(a => {
    console.error(`     - ${a.category}/${a.name}`);
  });
  process.exit(1);
}

// Check if animation exists
if (!asset.animations[animation]) {
  console.error(`❌ Animation not found: ${animation}`);
  console.error(`   Available animations for ${category}/${name}:`);
  Object.keys(asset.animations).forEach(anim => {
    console.error(`     - ${anim}`);
  });
  process.exit(1);
}

const animationConfig = asset.animations[animation];
const { frames, prompt } = animationConfig;

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
async function generateSpriteFrame(frameIndex, totalFrames) {
  const frameNumber = frameIndex + 1;
  const outputPath = path.join(assetsBaseDir, category, name, animation, `frame_${frameNumber.toString().padStart(2, '0')}.png`);
  
  // Ask user if they want to overwrite existing files
  if (fs.existsSync(outputPath)) {
    console.log(`⚠️  File already exists: ${outputPath}`);
    console.log(`   Skipping frame ${frameNumber}...`);
    return true;
  }

  try {
    console.log(`🎨 Generating frame ${frameNumber}/${totalFrames}...`);
    
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
        style_preset: "enhance"
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
    
    console.log(`✅ Generated frame_${frameNumber}.png`);
    
    // Rate limiting: wait 2 seconds between requests
    if (frameIndex < totalFrames - 1) {
      await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    return true;
  } catch (error) {
    if (error.response) {
      console.error(`❌ API Error (${error.response.status}):`, error.response.data?.message || error.message);
      if (error.response.status === 401) {
        console.error('   → Check if your API key is valid');
      } else if (error.response.status === 429) {
        console.error('   → Rate limit exceeded, waiting longer...');
        await new Promise(resolve => setTimeout(resolve, 5000));
        return false;
      }
    } else {
      console.error(`❌ Error generating frame ${frameNumber}:`, error.message);
    }
    return false;
  }
}

/**
 * Main generation function
 */
async function generateSingleAnimation() {
  console.log(`\n${'='.repeat(60)}`);
  console.log(`🎯 Generating: ${category}/${name}/${animation}`);
  console.log(`📝 Frames: ${frames}`);
  console.log(`🤖 Using: Stability AI (Stable Diffusion XL)`);
  console.log(`${'='.repeat(60)}\n`);
  
  // Create directories
  const animationDir = path.join(assetsBaseDir, category, name, animation);
  ensureDirectoryExists(animationDir);
  
  let generatedFrames = 0;
  
  // Generate each frame
  for (let frameIndex = 0; frameIndex < frames; frameIndex++) {
    const success = await generateSpriteFrame(frameIndex, frames);
    if (success) {
      generatedFrames++;
    }
  }
  
  console.log(`\n${'='.repeat(60)}`);
  console.log(`✅ Completed: ${generatedFrames}/${frames} frames generated`);
  console.log(`📂 Output: ${animationDir}`);
  console.log(`${'='.repeat(60)}\n`);
}

// Run the generator
generateSingleAnimation().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
