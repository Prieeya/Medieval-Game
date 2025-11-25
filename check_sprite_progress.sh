#!/bin/bash

# Script to check sprite generation progress

EXPECTED_TOTAL=437
CURRENT=$(find assets/images -name "*.png" 2>/dev/null | wc -l | tr -d ' ')
PERCENTAGE=$((CURRENT * 100 / EXPECTED_TOTAL))

echo "📊 Sprite Generation Progress:"
echo "   Generated: $CURRENT / $EXPECTED_TOTAL ($PERCENTAGE%)"
echo ""

if [ $CURRENT -ge $EXPECTED_TOTAL ]; then
    echo "✅ ALL SPRITES GENERATED!"
    echo "   Your game is ready with all animated sprites!"
else
    echo "⏳ Still generating... ($((EXPECTED_TOTAL - CURRENT)) remaining)"
    echo ""
    echo "Breakdown by category:"
    echo "   Towers:   $(find assets/images/towers -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
    echo "   Traps:   $(find assets/images/traps -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
    echo "   Enemies: $(find assets/images/enemies -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
    echo "   Bosses:  $(find assets/images/bosses -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
    echo "   Env:     $(find assets/images/environment -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
    echo "   Proj:    $(find assets/images/projectiles -name "*.png" 2>/dev/null | wc -l | tr -d ' ') frames"
fi

