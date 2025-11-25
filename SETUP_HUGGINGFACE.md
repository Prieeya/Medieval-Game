# 🚀 Quick Setup: Hugging Face (FREE)

## Step 1: Get Your Free API Token

1. **Sign up** (free): https://huggingface.co/join
2. **Get your token**: https://huggingface.co/settings/tokens
   - Click "New token"
   - Name it: "Sprite Generation"
   - Select "Read" permissions
   - Copy the token

## Step 2: Add Token to .env

```bash
# Add this line to your .env file
echo "HUGGINGFACE_API_KEY=your-token-here" >> .env
```

Or edit `.env` manually:
```
HUGGINGFACE_API_KEY=hf_your_actual_token_here
```

## Step 3: Test Connection

```bash
npm run test-hf
```

This will generate a test sprite to verify everything works.

## Step 4: Generate All Sprites

```bash
npm run generate:hf
```

## ⚠️ Important Notes

- **First Request**: May take 30-60 seconds (model loading)
- **Free Tier**: Has rate limits, but very generous
- **Model Loading**: If you get 503 error, wait 30 seconds and retry
- **Quality**: Same as Stability AI (Stable Diffusion XL)

## 🎯 Benefits

✅ **Completely FREE** (no credit card needed)  
✅ **Generous free tier**  
✅ **Same quality** as Stability AI  
✅ **Programmatic API** access  
✅ **No billing limits**  

## 📊 Comparison

| Feature | Stability AI | Hugging Face |
|---------|-------------|--------------|
| Cost | Paid | **FREE** |
| Quality | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| API | ✅ | ✅ |
| Free Tier | Limited | **Generous** |

## 🆘 Troubleshooting

**503 Error (Model Loading)**
→ Wait 30-60 seconds and try again. First request loads the model.

**401 Error (Invalid Key)**
→ Check your token at https://huggingface.co/settings/tokens

**429 Error (Rate Limit)**
→ Free tier has limits. Wait a few minutes and continue.

**Timeout**
→ Normal for first request. Model needs to load. Retry after 30 seconds.

## ✨ Ready!

Once you add your token, you can generate all 437 sprites for FREE!

