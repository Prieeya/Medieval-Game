# Stability AI Setup Guide

## ✅ Updated to Use Stability AI (Stable Diffusion)

The sprite generation pipeline has been updated to use **Stability AI** instead of OpenAI. This uses Stable Diffusion XL, a powerful open-source image generation model.

## 🔑 Get Your Free API Key

1. Visit: **https://platform.stability.ai/**
2. Sign up for a free account
3. Navigate to API Keys section
4. Generate a new API key
5. Copy the key

## ⚙️ Configure API Key

Edit the `.env` file and add your Stability AI API key:

```bash
STABILITY_API_KEY=sk-your-actual-api-key-here
```

Or set it as an environment variable:
```bash
export STABILITY_API_KEY="sk-your-actual-api-key-here"
```

## 🚀 Usage

Once configured, use the same commands:

```bash
# Test API connection
npm run test-api

# Generate all sprites
npm run generate

# Generate single animation
npm run generate:single towers arrow_tower idle
```

## 💰 Pricing

Stability AI offers:
- **Free tier**: Limited credits for testing
- **Pay-as-you-go**: Competitive pricing
- **No billing limit issues**: More flexible than OpenAI

Check current pricing at: https://platform.stability.ai/pricing

## 🎨 Model Details

- **Model**: Stable Diffusion XL 1024
- **Resolution**: 1024×1024
- **Quality**: High-quality sprite generation
- **Style**: Realistic medieval game sprites

## ✨ Benefits

- ✅ No billing limit issues
- ✅ High-quality image generation
- ✅ Free tier available
- ✅ Open-source model
- ✅ Better control over generation parameters

## 📝 Updated Files

- ✅ `generate_animated_sprites.mjs` - Now uses Stability AI
- ✅ `generate_single_animation.mjs` - Now uses Stability AI
- ✅ `test_api_connection.mjs` - Now uses Stability AI
- ✅ `package.json` - Updated dependencies (removed OpenAI, added axios)

## 🔧 Technical Details

The scripts now use:
- Stability AI REST API
- Stable Diffusion XL 1024 v1.0
- Negative prompts for better quality
- CFG scale: 7 (good balance)
- Steps: 30 (quality vs speed)
- Style preset: "enhance" (improves quality)

## ⚠️ Note

Make sure to:
1. Get your API key from https://platform.stability.ai/
2. Add it to `.env` file
3. Test connection with `npm run test-api`
4. Then start generating sprites!

