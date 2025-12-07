module Constants where

import Types

-- ============================================================================
-- Display Constants
-- ============================================================================

worldWidth, worldHeight :: Float
worldWidth = 1600
worldHeight = 900

worldLeft, worldRight, worldTop, worldBottom :: Float
worldLeft = -worldWidth / 2
worldRight = worldWidth / 2
worldTop = worldHeight / 2
worldBottom = -worldHeight / 2

-- ============================================================================
-- Fort Layout
-- ============================================================================

fortCenterX :: Float
fortCenterX = 400

fortWidth, fortHeight :: Float
fortWidth = 550  -- Increased from 400
fortHeight = 750  -- Increased from 600

fortLeft, fortRight, fortTop, fortBottom :: Float
fortLeft = fortCenterX - fortWidth / 2
fortRight = fortCenterX + fortWidth / 2
fortTop = fortHeight / 2
fortBottom = -fortHeight / 2

-- Gate positions for 3 gates on the left wall
gateX :: Float
gateX = fortLeft

-- Gate Y positions (top, center, bottom)
gate0Y, gate1Y, gate2Y :: Float
gate0Y = fortTop - 100     -- Top gate
gate1Y = 0                 -- Center gate
gate2Y = fortBottom + 100  -- Bottom gate

gateY :: Float
gateY = gate1Y  -- Default center gate (for backwards compatibility)

gateWidth :: Float
gateWidth = 80  -- Gate size (smaller since we have 3 now)

-- Get gate position by index
gatePositions :: [(Float, Float)]
gatePositions = [(gateX, gate0Y), (gateX, gate1Y), (gateX, gate2Y)]

-- ============================================================================
-- Castle Position
-- ============================================================================

castleX, castleY :: Float
castleX = fortRight - 100  -- Moved to right edge of fort with small gap from edge
castleY = 0

castleSize :: Float
castleSize = 180  -- Much larger castle size for impressive medieval look

-- ============================================================================
-- Wave & Level Constants
-- ============================================================================

wavesPerLevel :: Int
wavesPerLevel = 3

buildPhaseTime :: Float
buildPhaseTime = 30.0

bossPhaseTime :: Float
bossPhaseTime = 60.0

waveCountdownTime :: Float
waveCountdownTime = 0.0  -- No countdown - enemies spawn immediately

baseEnemyCount :: Int
baseEnemyCount = 8  -- Base enemy count for level 1

enemyCountScaling :: Float
enemyCountScaling = 1.4  -- More enemies each wave

-- Enemy count multiplier per level (level 1 = 1.0, level 2 = 1.5, level 3 = 2.0)
enemyCountPerLevel :: Int -> Float
enemyCountPerLevel level = 1.0 + fromIntegral (level - 1) * 0.5

-- Maximum level for victory
maxLevel :: Int
maxLevel = 3  -- Victory after completing level 3

-- ============================================================================
-- Spawn Zones
-- ============================================================================

leftSpawnX, centerSpawnX, rightSpawnX :: Float
leftSpawnX = -700
centerSpawnX = -550   -- Moved further left so enemies have longer path to center gate
rightSpawnX = -700    -- Same X as left, but different Y

spawnYRange :: (Float, Float)
spawnYRange = (-350, 350)

-- ============================================================================
-- Enemy Stats
-- ============================================================================

-- Base enemy stats (hp, armor, speed, attackRange, damage, attackCooldown)
-- These are scaled by level using enemyStatsForLevel
enemyStats :: UnitType -> (Float, Float, Float, Float, Float, Float)
-- Normal Enemies - Base stats for level 1
enemyStats GruntRaider = (60, 0, 60, 8, 20, 1.5)       -- Basic enemy
enemyStats BruteCrusher = (150, 6, 35, 10, 40, 2.5)    -- Tank - slow but tough
enemyStats Direwolf = (40, 0, 110, 6, 15, 1.0)         -- Fast but fragile
enemyStats Shieldbearer = (120, 10, 50, 8, 25, 2.0)    -- Armored unit
enemyStats Pyromancer = (70, 2, 45, 180, 18, 2.0)      -- Ranged caster with fire
enemyStats Necromancer = (90, 3, 40, 160, 15, 2.5)     -- Summoner
enemyStats TrapBreaker = (80, 5, 55, 8, 30, 1.5)       -- Anti-trap specialist
enemyStats WallClimber = (50, 0, 70, 8, 18, 1.2)       -- Climber
-- Short-range melee specialists (unlocked at higher levels)
enemyStats Berserker = (100, 3, 75, 12, 50, 1.0)       -- High damage, short range
enemyStats Assassin = (55, 0, 100, 10, 35, 0.8)        -- Fast short-range
enemyStats BoulderRamCrew = (250, 12, 40, 12, 70, 3.0) -- Siege unit - high HP, slow
-- Bosses - Appear at end of level 3
enemyStats IronbackMinotaur = (800, 20, 45, 15, 100, 2.0)  -- Tank boss
enemyStats FireDrake = (1000, 15, 55, 200, 120, 1.8)       -- Fire breath boss
enemyStats LichKingArcthros = (1200, 25, 40, 180, 100, 2.5) -- Summoner boss

-- Scale enemy stats based on level (enemies get stronger each level)
enemyStatsForLevel :: Int -> UnitType -> (Float, Float, Float, Float, Float, Float)
enemyStatsForLevel level ut =
  let (hp, armor, spd, range, dmg, cd) = enemyStats ut
      -- Scale factor increases with level: 1.0, 1.3, 1.6 for levels 1, 2, 3
      scaleFactor = 1.0 + fromIntegral (level - 1) * 0.3
      -- HP and damage scale more aggressively
      hpScale = 1.0 + fromIntegral (level - 1) * 0.4
      dmgScale = 1.0 + fromIntegral (level - 1) * 0.35
      -- Speed increases slightly
      spdScale = 1.0 + fromIntegral (level - 1) * 0.1
  in (hp * hpScale, armor * scaleFactor, spd * spdScale, range, dmg * dmgScale, cd)

-- (hp, armor, speed, attackRange, damage, attackCooldown)

enemyGoldValue :: UnitType -> Int
-- Normal Units
enemyGoldValue GruntRaider = 10      -- Basic Enemy
enemyGoldValue BruteCrusher = 20     -- Tank Enemy
enemyGoldValue Direwolf = 12         -- Fast Runner
enemyGoldValue Shieldbearer = 18     -- Armored Unit
enemyGoldValue Pyromancer = 25       -- Ranged Caster
enemyGoldValue Necromancer = 30      -- Summoner
enemyGoldValue TrapBreaker = 35        -- Anti-trap
enemyGoldValue WallClimber = 20        -- Climber
enemyGoldValue Berserker = 28          -- Short-range melee
enemyGoldValue Assassin = 22           -- Fast short-range
enemyGoldValue BoulderRamCrew = 40   -- Siege Unit
-- Boss Units (Every 3 Levels)
enemyGoldValue IronbackMinotaur = 100   -- Boss Level 3
enemyGoldValue FireDrake = 140          -- Boss Level 6
enemyGoldValue LichKingArcthros = 200  -- Boss Level 9

-- ============================================================================
-- Tower Stats
-- ============================================================================

towerCost :: TowerType -> Int
towerCost ArrowTower = 50      -- Basic Defense Tower
towerCost CatapultTower = 120   -- Siege Tower
towerCost CrossbowTower = 150   -- Sniper Tower
towerCost FireTower = 130      -- Burning Tower
towerCost TeslaTower = 200      -- Lightning Tower
towerCost BallistaTower = 220   -- Armor-Piercing Tower
towerCost PoisonTower = 90      -- Debuff Tower
towerCost BombardTower = 250    -- Cannon Tower

-- Tower upgrade cost: increases with level
towerUpgradeCost :: TowerType -> Int -> Int
towerUpgradeCost tt lvl = (towerCost tt * lvl) `div` 2 + 50

-- Max tower level
maxTowerLevel :: Int
maxTowerLevel = 3

-- Tower stats: (range, damage, fireRate)
towerStats :: TowerType -> (Float, Float, Float)
towerStats ArrowTower = (280, 40, 0.5)      -- Medium range, fast firing, good damage
towerStats CatapultTower = (400, 90, 1.5)   -- Long range, AoE
towerStats CrossbowTower = (420, 120, 1.0)  -- Very long range, precision, high damage
towerStats FireTower = (300, 30, 0.8)       -- Medium range, DoT
towerStats TeslaTower = (320, 55, 1.0)      -- Medium range, chain lightning
towerStats BallistaTower = (380, 140, 1.4)  -- Long range, piercing, very high damage
towerStats PoisonTower = (290, 25, 0.8)     -- Medium range, debuff
towerStats BombardTower = (380, 160, 2.2)   -- Long range, AoE burst, massive damage

-- Tower stats scaled by level (range, damage, fireRate)
towerStatsForLevel :: TowerType -> Int -> (Float, Float, Float)
towerStatsForLevel tt lvl =
  let (range, dmg, fr) = towerStats tt
      -- Level 1: base, Level 2: +25%, Level 3: +50%
      scale = 1.0 + fromIntegral (lvl - 1) * 0.25
      -- Fire rate improves (lower is faster)
      frScale = 1.0 - fromIntegral (lvl - 1) * 0.1
  in (range * scale, dmg * scale, fr * frScale)

-- Tower HP scaled by level
towerMaxHPForLevel :: TowerType -> Int -> Float
towerMaxHPForLevel tt lvl = towerMaxHP tt * (1.0 + fromIntegral (lvl - 1) * 0.3)

-- Tower upgrade bonuses description
towerUpgradeBonus :: Int -> String
towerUpgradeBonus 2 = "+25% Range/Damage, +10% Fire Rate, +30% HP"
towerUpgradeBonus 3 = "+50% Range/Damage, +20% Fire Rate, +60% HP, Special Ability"
towerUpgradeBonus _ = ""

-- Level 3 tower special abilities
towerLevel3Ability :: TowerType -> String
towerLevel3Ability ArrowTower = "Multi-shot (hits 2 targets)"
towerLevel3Ability CatapultTower = "Larger AoE radius"
towerLevel3Ability CrossbowTower = "Armor piercing"
towerLevel3Ability FireTower = "Fire spreads to nearby enemies"
towerLevel3Ability TeslaTower = "Chain to 3 enemies"
towerLevel3Ability BallistaTower = "Knockback effect"
towerLevel3Ability PoisonTower = "Poison lasts longer"
towerLevel3Ability BombardTower = "Stun on hit"

-- (range, damage, fireRate)

towerMaxHP :: TowerType -> Float
towerMaxHP ArrowTower = 180
towerMaxHP CatapultTower = 250
towerMaxHP CrossbowTower = 200
towerMaxHP FireTower = 220
towerMaxHP TeslaTower = 240
towerMaxHP BallistaTower = 280
towerMaxHP PoisonTower = 160
towerMaxHP BombardTower = 300

-- ============================================================================
-- Trap Stats
-- ============================================================================

trapCost :: TrapType -> Int
trapCost SpikeTrap = 20        -- Cheap early damage
trapCost FreezeTrap = 35        -- Stop fast units
trapCost FirePitTrap = 45      -- Continuous AoE
trapCost MagicSnareTrap = 50    -- Immobilize enemy
trapCost ExplosiveBarrel = 80  -- Player-triggered burst

-- Trap upgrade cost
trapUpgradeCost :: TrapType -> Int -> Int
trapUpgradeCost tt lvl = (trapCost tt * lvl) `div` 2 + 25

-- Max trap level
maxTrapLevel :: Int
maxTrapLevel = 3

-- Base trap damage
trapBaseDamage :: TrapType -> Float
trapBaseDamage SpikeTrap = 40       -- Instant damage
trapBaseDamage FreezeTrap = 5       -- Small damage + freeze/slow
trapBaseDamage FirePitTrap = 25     -- Per second damage
trapBaseDamage MagicSnareTrap = 10  -- Small damage + root
trapBaseDamage ExplosiveBarrel = 200  -- AoE burst

-- Trap damage scaled by level
trapDamageForLevel :: TrapType -> Int -> Float
trapDamageForLevel tt lvl = 
  let base = trapBaseDamage tt
      -- Level 1: base, Level 2: +40%, Level 3: +80%
      scale = 1.0 + fromIntegral (lvl - 1) * 0.4
  in base * scale

trapSlowFactor :: TrapType -> Float
trapSlowFactor FreezeTrap = 0.4  -- -60% slow
trapSlowFactor _ = 1.0

-- Trap slow factor scaled by level (lower = slower)
trapSlowFactorForLevel :: TrapType -> Int -> Float
trapSlowFactorForLevel tt lvl =
  let base = trapSlowFactor tt
      -- Each level reduces slow factor by 10% (more slow)
      reduction = fromIntegral (lvl - 1) * 0.1
  in max 0.2 (base - reduction)

-- Base trap HP (how much damage they can take before being destroyed)
trapBaseMaxHP :: TrapType -> Float
trapBaseMaxHP SpikeTrap = 30        -- Fragile
trapBaseMaxHP FreezeTrap = 50       -- Moderate
trapBaseMaxHP FirePitTrap = 80      -- Sturdy
trapBaseMaxHP MagicSnareTrap = 40   -- Moderate
trapBaseMaxHP ExplosiveBarrel = 20  -- Very fragile (explodes easily)

-- Trap HP scaled by level
trapMaxHPForLevel :: TrapType -> Int -> Float
trapMaxHPForLevel tt lvl = trapBaseMaxHP tt * (1.0 + fromIntegral (lvl - 1) * 0.5)

-- Trap upgrade bonuses description
trapUpgradeBonus :: Int -> String
trapUpgradeBonus 2 = "+40% Damage, +50% HP, Improved Effect"
trapUpgradeBonus 3 = "+80% Damage, +100% HP, Special Ability"
trapUpgradeBonus _ = ""

-- Level 3 trap special abilities
trapLevel3Ability :: TrapType -> String
trapLevel3Ability SpikeTrap = "Bleeding (DoT for 3s)"
trapLevel3Ability FreezeTrap = "Complete freeze for 2s"
trapLevel3Ability FirePitTrap = "Larger radius, fire spreads"
trapLevel3Ability MagicSnareTrap = "Drains enemy HP"
trapLevel3Ability ExplosiveBarrel = "Chain explosion"

-- For backwards compatibility
trapMaxHP :: TrapType -> Float
trapMaxHP = trapBaseMaxHP

-- ============================================================================
-- Ability Stats
-- ============================================================================

abilityCooldowns :: AbilityType -> Float
abilityCooldowns Firestorm = 60.0
abilityCooldowns FreezeField = 45.0
abilityCooldowns RepairWalls = 30.0
abilityCooldowns TimeSlow = 50.0

abilityDurations :: AbilityType -> Float
abilityDurations Firestorm = 3.0
abilityDurations FreezeField = 5.0
abilityDurations RepairWalls = 0.0
abilityDurations TimeSlow = 8.0

-- ============================================================================
-- Fort Health
-- ============================================================================

gateMaxHP :: Float
gateMaxHP = 3000  -- Base gate HP

gateUpgradeBaseCost :: Int
gateUpgradeBaseCost = 300  -- Base cost for upgrading gate

gateHPPerLevel :: Float
gateHPPerLevel = 1000  -- HP increase per level (more significant upgrades)

-- Base repair cost per 100 HP of damage
gateRepairCostPerHP :: Float
gateRepairCostPerHP = 0.05  -- 5 gold per 100 HP damage (50 gold per 1000 HP)

-- Minimum repair cost
gateRepairMinCost :: Int
gateRepairMinCost = 25

-- Bonus cost if gate is completely destroyed
gateDestroyedBonus :: Int
gateDestroyedBonus = 100  -- Extra cost to rebuild from scratch

wallMaxHP :: Float
wallMaxHP = 600

castleMaxHP :: Float
castleMaxHP = 2000

-- ============================================================================
-- Resource Constants
-- ============================================================================

startingGold :: Int
startingGold = 750  -- More starting gold for building initial defenses


baseWaveGold :: Int
baseWaveGold = 200

repairCost :: Float -> Int
repairCost damage = ceiling (damage * 0.5)

-- ============================================================================
-- Gameplay Tuning
-- ============================================================================

projectileSpeed :: Float
projectileSpeed = 300.0

enemyDetectionRange :: Float
enemyDetectionRange = 50.0

climbDuration :: Float
climbDuration = 3.0

burnDamagePerSecond :: Float
burnDamagePerSecond = 10.0

burnDuration :: Float
burnDuration = 3.0

iceSlowFactor :: Float
iceSlowFactor = 0.5

iceDuration :: Float
iceDuration = 2.0

lightningChainRange :: Float
lightningChainRange = 100.0

lightningMaxChains :: Int
lightningMaxChains = 3

-- ============================================================================
-- Visual Scaling Constants
-- ============================================================================

-- Global pixel art scaling factor
globalPixelScale :: Float
globalPixelScale = 1.4

-- Entity scaling factors (applied on top of globalPixelScale)
enemyScale :: Float
enemyScale = 1.4  -- 40% bigger

towerScale :: Float
towerScale = 1.2  -- Reduced from 1.4 (defenses smaller)

trapScale :: Float
trapScale = 0.1  -- Traps are smaller (reduced from 0.2)

projectileScale :: Float
projectileScale = 2.0  -- Reduced from 3.5 (bullets smaller)

bossScale :: Float
bossScale = 2.0  -- Bosses are 2x enemy scale

-- Base sprite sizes (before scaling)
enemyBaseSize :: Float
enemyBaseSize = 48.0  -- 48x48 pixel sprites

towerBaseSize :: Float
towerBaseSize = 48.0  -- 48x48 pixel sprites

projectileBaseSize :: Float
projectileBaseSize = 24.0  -- 24x24 pixel sprites

tileSize :: Float
tileSize = 64.0  -- 64x64 game tiles (32x32 sprites scaled 2x)