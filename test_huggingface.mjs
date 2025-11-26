#!/usr/bin/env node

/**
 * Test script to verify Hugging Face API connection
 */

import 'dotenv/config';
import { HfInference } from '@huggingface/inference';
import sharp from 'sharp';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const HF_API_KEY = process.env.HUGGINGFACE_API_KEY;
const MODEL_NAME = 'stabilityai/stable-diffusion-xl-base-1.0';

// Initialize Hugging Face Inference client
const hf = new HfInference(HF_API_KEY);

if (!HF_API_KEY) {
  console.error('❌ ERROR: HUGGINGFACE_API_KEY not found in .env file');
  console.error('   Please add: HUGGINGFACE_API_KEY="your-token"');
  console.error('   Get your free token at: https://huggingface.co/settings/tokens');
  process.exit(1);
}

console.log('🔍 Testing Hugging Face API connection...\n');
console.log(`📝 API Key: ${HF_API_KEY.substring(0, 20)}...`);
console.log(`🤖 Model: Stable Diffusion XL (via Hugging Face)\n`);

async function testConnection() {
  try {
    console.log('🎨 Generating test sprite (this may take 20-40 seconds)...\n');
    console.log('   Note: First request may take longer as model loads\n');
    
    const testPrompt = "Top-down 2D orthographic game sprite, realistic medieval style, wooden arrow, transparent background, no perspective, no 3D, slightly stylized, test frame";
    const negativePrompt = "3D, perspective, isometric, low quality, blurry, distorted, watermark, text, signature";
    
    const imageBlob = await hf.textToImage({
      model: MODEL_NAME,
      inputs: testPrompt,
      parameters: {
        negative_prompt: negativePrompt,
        num_inference_steps: 30,
        guidance_scale: 7.5,
        width: 1024,
        height: 1024
      }
    });

    if (imageBlob) {
      // Save test image
      const testDir = path.join(__dirname, 'assets', 'images', 'test');
      if (!fs.existsSync(testDir)) {
        fs.mkdirSync(testDir, { recursive: true });
      }
      const testPath = path.join(testDir, 'test_sprite_hf.png');
      
      const arrayBuffer = await imageBlob.arrayBuffer();
      const imageBuffer = Buffer.from(arrayBuffer);
      await sharp(imageBuffer)
        .trim({ threshold: 10 })
        .png()
        .toFile(testPath);
      
      console.log('✅ API connection successful!');
      console.log('✅ Image generated successfully!');
      console.log(`📁 Test image saved to: ${testPath}`);
      console.log('\n✨ Your Hugging Face sprite generation is ready!\n');
      console.log('Run: npm run generate:hf');
      return true;
    } else {
      console.error('❌ Invalid response from API');
      return false;
    }
  } catch (error) {
    if (error.status) {
      const status = error.status;
      
      console.error('❌ API Error:', status, error.statusText || '');
      
      if (status === 503) {
        console.error('   → Model is loading. This is normal for first request.');
        console.error('   → Wait 30-60 seconds and try again.');
        console.error('   → Or the model may need to be loaded: https://huggingface.co/stabilityai/stable-diffusion-xl-base-1.0');
      } else if (status === 401) {
        console.error('   → Invalid API key. Check your HUGGINGFACE_API_KEY');
        console.error('   → Get your token at: https://huggingface.co/settings/tokens');
      } else if (status === 429) {
        console.error('   → Rate limit exceeded. Free tier has limits.');
        console.error('   → Wait a few minutes and try again.');
      } else {
        console.error('   Error:', error.message?.substring(0, 200) || 'No details');
      }
    } else if (error.code === 'ECONNABORTED') {
      console.error('❌ Request timeout. Model may be loading.');
      console.error('   → This is normal for first request. Try again in 30 seconds.');
    } else {
      console.error('❌ Error:', error.message || 'Unknown error');
    }
    return false;
  }
}

testConnection().then(success => {
  process.exit(success ? 0 : 1);
});

