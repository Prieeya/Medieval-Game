#!/usr/bin/env node

/**
 * Sprite generation using Hugging Face Inference API (FREE TIER)
 * Sign up at: https://huggingface.co/
 * Get API token: https://huggingface.co/settings/tokens
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
const HF_API_KEY = process.env.HUGGINGFACE_API_KEY;
const HF_API_URL = 'https://api-inference.huggingface.co/models/stabilityai/stable-diffusion-xl-base-1.0';

if (!HF_API_KEY) {
  console.error('❌ ERROR: HUGGINGFACE_API_KEY environment variable not set!');
  console.error('   Please set it in .env file: HUGGINGFACE_API_KEY="your-token"');
  console.error('   Get your free token at: https://huggingface.co/settings/tokens');
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
 * Generate a single sprite frame using Hugging Face API
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
    const negativePrompt = "3D, perspective, isometric, low quality, blurry, distorted, watermark, text, signature";
    
    // Call Hugging Face Inference API
    const response = await axios.post(
      HF_API_URL,
      {
        inputs: enhancedPrompt,
        parameters: {
          negative_prompt: negativePrompt,
          num_inference_steps: 30,
          guidance_scale: 7.5,
          width: 1024,
          height: 1024
        }
      },
      {
        headers: {
          'Authorization': `Bearer ${HF_API_KEY}`,
          'Content-Type': 'application/json'
        },
        responseType: 'arraybuffer',
        timeout: 60000  // 60 second timeout
      }
    );

    if (!response.data) {
      throw new Error('Invalid response from Hugging Face API');
    }

    // Hugging Face returns image directly as binary
    const imageBuffer = Buffer.from(response.data);
    
    // Process with sharp: trim transparent borders and save
    await sharp(imageBuffer)
      .trim({ threshold: 10 })
      .png()
      .toFile(outputPath);
    
    console.log(`✅ Generated ${category}/${name}/${animation}/frame_${frameNumber}.png`);
    
    // Rate limiting: wait 2 seconds between requests
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    return true;
  } catch (error) {
    if (error.response) {
      const status = error.response.status;
      const data = error.response.data;
      
      if (status === 503) {
        // Model is loading, wait and retry
        console.log(`⏳ Model loading, waiting 10 seconds...`);
        await new Promise(resolve => setTimeout(resolve, 10000));
        return false; // Retry
      } else if (status === 429) {
        console.error(`❌ Rate limit exceeded, waiting 5 seconds...`);
        await new Promise(resolve => setTimeout(resolve, 5000));
        return false; // Retry
      } else if (status === 401) {
        console.error(`❌ Invalid API key. Check your HUGGINGFACE_API_KEY`);
        return false;
      } else {
        console.error(`❌ API Error (${status}):`, data?.toString().substring(0, 200) || error.message);
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
      let retries = 3;
      let success = false;
      
      while (retries > 0 && !success) {
        success = await generateSpriteFrame(
          category,
          name,
          animationName,
          frameIndex,
          frames,
          prompt
        );
        if (!success) {
          retries--;
          if (retries > 0) {
            console.log(`   Retrying... (${retries} attempts left)`);
          }
        }
      }
      
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
  console.log('🚀 Starting sprite generation pipeline (Hugging Face)...\n');
  console.log(`📂 Output directory: ${assetsBaseDir}\n`);
  console.log(`🤖 Using: Hugging Face Inference API (Stable Diffusion XL)\n`);
  
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

