#!/bin/bash

# Monitor sprite generation and notify when complete

EXPECTED_TOTAL=437
CHECK_INTERVAL=30  # Check every 30 seconds

echo "🔍 Monitoring sprite generation..."
echo "   Expected total: $EXPECTED_TOTAL frames"
echo "   Checking every $CHECK_INTERVAL seconds"
echo "   Press Ctrl+C to stop monitoring"
echo ""

while true; do
    CURRENT=$(find assets/images -name "*.png" 2>/dev/null | wc -l | tr -d ' ')
    PERCENTAGE=$((CURRENT * 100 / EXPECTED_TOTAL))
    REMAINING=$((EXPECTED_TOTAL - CURRENT))
    
    TIMESTAMP=$(date '+%H:%M:%S')
    echo "[$TIMESTAMP] Progress: $CURRENT/$EXPECTED_TOTAL ($PERCENTAGE%) - $REMAINING remaining"
    
    if [ $CURRENT -ge $EXPECTED_TOTAL ]; then
        echo ""
        echo "╔══════════════════════════════════════════════════════════════╗"
        echo "║                                                              ║"
        echo "║        ✅ ALL SPRITES GENERATED SUCCESSFULLY! ✅            ║"
        echo "║                                                              ║"
        echo "╚══════════════════════════════════════════════════════════════╝"
        echo ""
        echo "🎮 Your game is now ready with all animated sprites!"
        echo "   Run: cabal build && cabal run medieval-siege"
        echo ""
        
        # Optional: Play notification sound (macOS)
        if command -v say >/dev/null 2>&1; then
            say "Sprite generation complete"
        fi
        
        exit 0
    fi
    
    sleep $CHECK_INTERVAL
done

