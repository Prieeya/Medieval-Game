#!/bin/bash

# Verification script for sprite generation pipeline setup

echo "🔍 Verifying Sprite Generation Pipeline Setup..."
echo "=================================================="
echo ""

# Check Node.js
echo -n "Checking Node.js... "
if command -v node >/dev/null 2>&1; then
    NODE_VERSION=$(node --version)
    echo "✅ Found: $NODE_VERSION"
else
    echo "❌ Not found"
    echo "   → Install from: https://nodejs.org/"
fi

# Check npm
echo -n "Checking npm... "
if command -v npm >/dev/null 2>&1; then
    NPM_VERSION=$(npm --version)
    echo "✅ Found: $NPM_VERSION"
else
    echo "❌ Not found"
    echo "   → Install Node.js (includes npm)"
fi

# Check .env file
echo -n "Checking .env file... "
if [ -f ".env" ]; then
    if grep -q "OPENAI_API_KEY=" .env; then
        API_KEY=$(grep "OPENAI_API_KEY=" .env | cut -d'=' -f2 | cut -c1-20)
        echo "✅ Found (Key: ${API_KEY}...)"
    else
        echo "❌ Missing OPENAI_API_KEY"
    fi
else
    echo "❌ Not found"
fi

# Check package.json
echo -n "Checking package.json... "
if [ -f "package.json" ]; then
    echo "✅ Found"
else
    echo "❌ Not found"
fi

# Check generation scripts
echo -n "Checking generate_animated_sprites.mjs... "
if [ -f "generate_animated_sprites.mjs" ]; then
    echo "✅ Found"
else
    echo "❌ Not found"
fi

echo -n "Checking generate_single_animation.mjs... "
if [ -f "generate_single_animation.mjs" ]; then
    echo "✅ Found"
else
    echo "❌ Not found"
fi

echo -n "Checking test_api_connection.mjs... "
if [ -f "test_api_connection.mjs" ]; then
    echo "✅ Found"
else
    echo "❌ Not found"
fi

echo -n "Checking assetsConfigAnimated.mjs... "
if [ -f "assetsConfigAnimated.mjs" ]; then
    echo "✅ Found"
else
    echo "❌ Not found"
fi

# Check node_modules
echo -n "Checking node_modules... "
if [ -d "node_modules" ] && [ "$(ls -A node_modules 2>/dev/null)" ]; then
    echo "✅ Installed"
else
    echo "⚠️  Not installed (run: npm install)"
fi

# Check asset directories
echo -n "Checking assets/images directories... "
if [ -d "assets/images" ]; then
    DIRS=$(find assets/images -mindepth 1 -maxdepth 1 -type d | wc -l)
    echo "✅ Found ($DIRS categories)"
else
    echo "❌ Not found"
fi

echo ""
echo "=================================================="
echo "📋 Next Steps:"
echo ""
if ! command -v node >/dev/null 2>&1; then
    echo "1. Install Node.js: https://nodejs.org/"
    echo "2. Run: npm install"
    echo "3. Run: npm run test-api"
    echo "4. Run: npm run generate"
else
    if [ ! -d "node_modules" ] || [ -z "$(ls -A node_modules 2>/dev/null)" ]; then
        echo "1. Run: npm install"
        echo "2. Run: npm run test-api"
        echo "3. Run: npm run generate"
    else
        echo "1. Run: npm run test-api"
        echo "2. Run: npm run generate"
    fi
fi
echo ""

