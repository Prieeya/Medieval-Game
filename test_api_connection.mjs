#!/usr/bin/env node

/**
 * Test script to verify Stability AI API connection
 */

import 'dotenv/config';
import axios from 'axios';
import sharp from 'sharp';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const STABILITY_API_KEY = process.env.STABILITY_API_KEY;
const STABILITY_API_URL = 'https://api.stability.ai/v1/generation/stable-diffusion-xl-1024-v1-0/text-to-image';

if (!STABILITY_API_KEY) {
  console.error('❌ ERROR: STABILITY_API_KEY not found in .env file');
  console.error('   Please add: STABILITY_API_KEY="your-api-key"');
  console.error('   Get your free API key at: https://platform.stability.ai/');
  process.exit(1);
}

console.log('🔍 Testing Stability AI API connection...\n');
console.log(`📝 API Key: ${STABILITY_API_KEY.substring(0, 20)}...`);
console.log(`🤖 Model: Stable Diffusion XL 1024\n`);

async function testConnection() {
  try {
    console.log('🎨 Generating test sprite (this may take 15-30 seconds)...\n');
    
    const testPrompt = "Top-down 2D orthographic game sprite, realistic medieval style, wooden arrow, transparent background, no perspective, no 3D, slightly stylized, test frame";
    const negativePrompt = "3D, perspective, isometric, low quality, blurry, distorted, watermark, text, signature";
    
    const response = await axios.post(
      STABILITY_API_URL,
      {
        text_prompts: [
          {
            text: testPrompt,
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

    if (response.data && response.data.artifacts && response.data.artifacts.length > 0) {
      // Stability AI returns base64 encoded images in artifacts array
      const artifact = response.data.artifacts[0];
      const base64Image = artifact.base64;
      
      if (!base64Image) {
        throw new Error('No image data in response');
      }
      
      // Save test image
      const testDir = path.join(__dirname, 'assets', 'images', 'test');
      if (!fs.existsSync(testDir)) {
        fs.mkdirSync(testDir, { recursive: true });
      }
      const testPath = path.join(testDir, 'test_sprite.png');
      
      const imageBuffer = Buffer.from(base64Image, 'base64');
      await sharp(imageBuffer)
        .trim({ threshold: 10 })
        .png()
        .toFile(testPath);
      
      console.log('✅ API connection successful!');
      console.log('✅ Image generated successfully!');
      console.log(`📁 Test image saved to: ${testPath}`);
      console.log('\n✨ Your sprite generation pipeline is ready to use!\n');
      console.log('Run: npm run generate');
      return true;
    } else {
      console.error('❌ Invalid response from API');
      return false;
    }
  } catch (error) {
    if (error.response) {
      console.error('❌ API Error:', error.response.status, error.response.statusText);
      console.error('   Response:', error.response.data?.toString().substring(0, 200) || 'No details');
      
      if (error.response.status === 401) {
        console.error('   → Check if your API key is valid');
        console.error('   → Get your API key at: https://platform.stability.ai/');
      } else if (error.response.status === 402) {
        console.error('   → Payment required. Check your account balance.');
      } else if (error.response.status === 429) {
        console.error('   → Rate limit exceeded, please wait');
      } else if (error.response.status === 400) {
        console.error('   → Bad request. Check your API endpoint and parameters.');
      }
    } else {
      console.error('❌ Network Error:', error.message);
    }
    return false;
  }
}

testConnection().then(success => {
  process.exit(success ? 0 : 1);
});
