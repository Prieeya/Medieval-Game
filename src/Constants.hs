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
baseEnemyCount = 6  -- Reduced enemy count for balanced gameplay

enemyCountScaling :: Float
enemyCountScaling = 1.2  -- Slower scaling for more manageable waves

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

enemyStats :: UnitType -> (Float, Float, Float, Float, Float, Float)
-- Normal Enemies - Balanced for fun gameplay
-- (hp, armor, speed, attackRange, damage, attackCooldown)
enemyStats GruntRaider = (50, 0, 65, 8, 25, 1.5)       -- Basic enemy - low stats
enemyStats BruteCrusher = (120, 5, 40, 10, 45, 2.5)    -- Tank - slow but tough
enemyStats Direwolf = (35, 0, 120, 6, 18, 1.0)         -- Fast but fragile
enemyStats Shieldbearer = (100, 8, 55, 8, 30, 2.0)     -- Armored unit
enemyStats Pyromancer = (60, 2, 50, 180, 15, 2.0)      -- Ranged caster - low damage
enemyStats Necromancer = (80, 3, 45, 160, 18, 2.5)     -- Summoner
enemyStats TrapBreaker = (70, 4, 60, 8, 35, 1.5)       -- Anti-trap specialist
enemyStats WallClimber = (45, 0, 75, 8, 22, 1.2)       -- Climber - fast but weak
-- Short-range melee specialists
enemyStats Berserker = (90, 2, 80, 12, 60, 1.0)        -- High damage, short range
enemyStats Assassin = (50, 0, 110, 10, 40, 0.8)        -- Fast short-range
enemyStats BoulderRamCrew = (200, 10, 45, 12, 80, 3.0) -- Siege unit - high HP, slow
-- Bosses (Every 3 Levels) - Made much harder
-- Bosses - Challenging but beatable
enemyStats IronbackMinotaur = (600, 15, 50, 15, 80, 2.5)   -- Tank boss - tough but manageable
enemyStats FireDrake = (750, 12, 60, 200, 100, 2.0)        -- Ranged fire boss
enemyStats LichKingArcthros = (900, 18, 45, 180, 90, 3.0)  -- Summoner boss

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

towerUpgradeCost :: TowerType -> Int -> Int
towerUpgradeCost tt lvl = towerCost tt * lvl

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

trapDamage :: TrapType -> Float
trapDamage SpikeTrap = 40       -- Instant damage
trapDamage FreezeTrap = 0      -- No damage, just freeze/slow
trapDamage FirePitTrap = 25    -- Per second damage
trapDamage MagicSnareTrap = 0   -- No damage, just root
trapDamage ExplosiveBarrel = 200  -- AoE burst

trapSlowFactor :: TrapType -> Float
trapSlowFactor FreezeTrap = 0.4  -- -60% slow
trapSlowFactor _ = 1.0

-- Trap HP (how much damage they can take before being destroyed)
trapMaxHP :: TrapType -> Float
trapMaxHP SpikeTrap = 30        -- Fragile
trapMaxHP FreezeTrap = 50       -- Moderate
trapMaxHP FirePitTrap = 80      -- Sturdy
trapMaxHP MagicSnareTrap = 40   -- Moderate
trapMaxHP ExplosiveBarrel = 20  -- Very fragile (explodes easily)

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
gateMaxHP = 5000  -- Increased gate HP

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