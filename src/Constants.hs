module Constants where

import Types hiding (towerMaxHP, trapMaxHP)

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
baseEnemyCount = 10  -- Base enemy count for level 1 (increased)

enemyCountScaling :: Float
enemyCountScaling = 1.5  -- More enemies each wave (increased)

-- Enemy count multiplier per level (level 1 = 1.0, level 2 = 1.7, level 3 = 2.4)
enemyCountPerLevel :: Int -> Float
enemyCountPerLevel level = 1.0 + fromIntegral (level - 1) * 0.7

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
-- MUCH HARDER ENEMIES - Significantly increased HP and damage
enemyStats :: UnitType -> (Float, Float, Float, Float, Float, Float)
-- Normal Enemies - Base stats for level 1 (MUCH HARDER)
enemyStats GruntRaider = (120, 2, 65, 8, 45, 1.3)       -- Basic enemy - +50% HP, +50% damage
enemyStats BruteCrusher = (300, 10, 40, 10, 85, 2.2)     -- Tank - +50% HP, +55% damage
enemyStats Direwolf = (85, 0, 120, 6, 35, 0.9)          -- Fast - +55% HP, +59% damage
enemyStats Shieldbearer = (240, 15, 55, 8, 55, 1.8)     -- Armored - +50% HP, +57% damage
enemyStats Pyromancer = (140, 3, 50, 180, 45, 1.8)      -- Ranged - +56% HP, +61% damage
enemyStats Necromancer = (170, 5, 45, 160, 35, 2.2)     -- Summoner - +55% HP, +59% damage
enemyStats TrapBreaker = (150, 8, 60, 8, 60, 1.3)       -- Anti-trap - +50% HP, +50% damage
enemyStats WallClimber = (110, 2, 80, 8, 42, 1.0)       -- Climber - +57% HP, +50% damage
-- Short-range melee specialists (unlocked at higher levels)
enemyStats Berserker = (200, 5, 85, 12, 105, 0.8)       -- High damage - +54% HP, +50% damage
enemyStats Assassin = (115, 0, 115, 10, 75, 0.6)         -- Fast - +53% HP, +50% damage
enemyStats BoulderRamCrew = (525, 18, 45, 12, 150, 2.5) -- Siege - +50% HP, +50% damage
-- Bosses - Appear at end of level 3 (MUCH HARDER)
enemyStats IronbackMinotaur = (2250, 30, 50, 15, 225, 1.8)  -- Tank boss - +50% HP, +50% damage
enemyStats FireDrake = (2700, 22, 60, 200, 270, 1.5)        -- Fire breath boss - +50% HP, +50% damage
enemyStats LichKingArcthros = (3300, 35, 45, 180, 210, 2.0) -- Summoner boss - +50% HP, +50% damage

-- Scale enemy stats based on level (enemies get stronger each level)
-- MORE AGGRESSIVE SCALING
enemyStatsForLevel :: Int -> UnitType -> (Float, Float, Float, Float, Float, Float)
enemyStatsForLevel level ut =
  let (hp, armor, spd, range, dmg, cd) = enemyStats ut
      -- Scale factor increases with level: 1.0, 1.5, 2.0 for levels 1, 2, 3
      scaleFactor = 1.0 + fromIntegral (level - 1) * 0.5
      -- HP and damage scale more aggressively
      hpScale = 1.0 + fromIntegral (level - 1) * 0.6
      dmgScale = 1.0 + fromIntegral (level - 1) * 0.5
      -- Speed increases slightly
      spdScale = 1.0 + fromIntegral (level - 1) * 0.15
      -- Attack cooldown decreases (faster attacks)
      cdScale = 1.0 - fromIntegral (level - 1) * 0.1
  in (hp * hpScale, armor * scaleFactor, spd * spdScale, range, dmg * dmgScale, max 0.5 (cd * cdScale))

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

-- Tower costs based on damage output (costlier for higher damage)
-- Formula: base cost + (damage * cost per damage point)
towerCost :: TowerType -> Int
towerCost ArrowTower = 50 + round (40 * 1.2)      -- 40 damage -> 50g (was 50g)
towerCost CatapultTower = 80 + round (90 * 1.0)   -- 90 damage -> 170g (was 120g)
towerCost CrossbowTower = 100 + round (120 * 1.0) -- 120 damage -> 180g (was 150g)
towerCost FireTower = 70 + round (30 * 1.5)      -- 30 damage + DoT -> 115g (was 130g)
towerCost TeslaTower = 120 + round (55 * 1.2)    -- 55 damage + chain -> 186g (was 200g)
towerCost BallistaTower = 150 + round (140 * 1.0) -- 140 damage -> 290g (was 220g)
towerCost PoisonTower = 60 + round (25 * 1.2)     -- 25 damage + debuff -> 90g (was 90g)
towerCost BombardTower = 180 + round (160 * 1.0)  -- 160 damage + AoE -> 250g (was 250g)

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

-- Max gate level
maxGateLevel :: Int
maxGateLevel = 5

-- Gate upgrade cost (increases with level)
gateUpgradeCost :: Int -> Int
gateUpgradeCost lvl = 150 + lvl * 100  -- 250, 350, 450, 550 for levels 2-5

gateUpgradeBaseCost :: Int
gateUpgradeBaseCost = 150  -- Base cost for upgrading gate (for backwards compat)

gateHPPerLevel :: Float
gateHPPerLevel = 800  -- HP increase per level

-- Gate armor per level (reduces damage taken)
gateArmorPerLevel :: Float
gateArmorPerLevel = 5  -- +5 armor per level

-- Gate upgrade bonuses description
gateUpgradeBonus :: Int -> String
gateUpgradeBonus 2 = "+800 HP, +5 Armor"
gateUpgradeBonus 3 = "+1600 HP, +10 Armor, Reinforced"
gateUpgradeBonus 4 = "+2400 HP, +15 Armor, Auto-repair"
gateUpgradeBonus 5 = "+3200 HP, +20 Armor, Invulnerable 5s"
gateUpgradeBonus _ = ""

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
startingGold = 500  -- Starting gold


baseWaveGold :: Int
baseWaveGold = 200  -- Increased from 200 to provide more gold for tower placement

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