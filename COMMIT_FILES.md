# Files Changed for Each Commit

## 1. Increase gold rewards after waves
**Files to commit:**
- `src/Constants.hs` (baseWaveGold constant)
- `src/Systems/WaveSystem.hs` (gold reward formula)

**Command:**
```bash
git add src/Constants.hs src/Systems/WaveSystem.hs
git commit -m "Increase gold rewards to help players afford towers before boss waves"
```

---

## 2. Fix tower keyboard shortcuts
**Files to commit:**
- `src/Input/InputHandler.hs` (tower key mappings)

**Command:**
```bash
git add src/Input/InputHandler.hs
git commit -m "Fix tower keyboard shortcuts to match UI hotbar labels"
```

---

## 3. Remove tower unlock system
**Files to commit:**
- `src/Input/InputHandler.hs` (removed lock check from placeTower)
- `src/Rendering/RenderWorld.hs` (removed lock-related preview code)

**Command:**
```bash
git add src/Input/InputHandler.hs src/Rendering/RenderWorld.hs
git commit -m "Remove tower unlock requirement - all towers available from start"
```

---

## 4. Simplify gate repair system
**Files to commit:**
- `src/Input/InputHandler.hs` (repairGateIfPending function)

**Command:**
```bash
git add src/Input/InputHandler.hs
git commit -m "Change gate repair to flat 150 gold per destroyed gate"
```

---

## 5. Fix UI button positioning
**Files to commit:**
- `src/Rendering/RenderUI.hs` (renderQuitResetButtons function)
- `src/Input/InputHandler.hs` (button click detection)

**Command:**
```bash
git add src/Rendering/RenderUI.hs src/Input/InputHandler.hs
git commit -m "Reposition quit and reset buttons in upper right corner"
```

---

## 6. Adjust castle HP bar position
**Files to commit:**
- `src/Rendering/RenderUI.hs` (renderCastleHPBar function)

**Command:**
```bash
git add src/Rendering/RenderUI.hs
git commit -m "Move castle HP bar down to avoid blocking UI buttons"
```

---

## 7. Add comprehensive README
**Files to commit:**
- `README.md` (new file)

**Command:**
```bash
git add README.md
git commit -m "Add README with gameplay guide and project structure"
```

---

## All changes at once (alternative)
**Files to commit:**
- `src/Constants.hs`
- `src/Systems/WaveSystem.hs`
- `src/Input/InputHandler.hs`
- `src/Rendering/RenderWorld.hs`
- `src/Rendering/RenderUI.hs`
- `README.md`

**Command:**
```bash
git add src/Constants.hs src/Systems/WaveSystem.hs src/Input/InputHandler.hs src/Rendering/RenderWorld.hs src/Rendering/RenderUI.hs README.md
git commit -m "Improve game balance and UI, fix controls, add documentation"
```
