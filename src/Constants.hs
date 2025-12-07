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
baseEnemyCount = 10

enemyCountScaling :: Float
enemyCountScaling = 1.5

-- ============================================================================
-- Spawn Zones
-- ============================================================================

leftSpawnX, centerSpawnX, rightSpawnX :: Float
leftSpawnX = -700
centerSpawnX = -400
rightSpawnX = -100

spawnYRange :: (Float, Float)
spawnYRange = (-350, 350)

-- ============================================================================
-- Enemy Stats
-- ============================================================================

enemyStats :: UnitType -> (Float, Float, Float, Float, Float, Float)
-- Normal Enemies - Made harder
enemyStats GruntRaider = (90, 2, 75, 8, 55, 1.3)      -- Increased HP 60->90, added armor, more damage
enemyStats BruteCrusher = (220, 8, 45, 10, 95, 2.2)    -- Increased HP 150->220, more armor, more damage
enemyStats Direwolf = (60, 1, 150, 6, 35, 0.9)        -- Increased HP 40->60, faster, more damage
enemyStats Shieldbearer = (180, 12, 65, 8, 55, 1.8)     -- Increased HP 120->180, more armor, more damage
enemyStats Pyromancer = (120, 4, 60, 200, 25, 1.8)      -- Increased HP 80->120, more damage
enemyStats Necromancer = (150, 5, 55, 180, 30, 2.2)    -- Increased HP 100->150, more damage
enemyStats TrapBreaker = (130, 6, 70, 8, 65, 1.3)       -- Increased HP 90->130, more damage
enemyStats WallClimber = (70, 1, 85, 8, 40, 0.9)       -- Increased HP 50->70, faster, more damage
-- Short-range melee specialists
enemyStats Berserker = (160, 3, 90, 12, 120, 0.8)      -- High damage, short range, fast attack, can climb walls
enemyStats Assassin = (80, 0, 130, 10, 80, 0.6)        -- Low HP, very fast, short range, targets towers inside fort
enemyStats BoulderRamCrew = (350, 15, 55, 12, 180, 2.8) -- Increased HP 250->350, more armor, more damage
-- Bosses (Every 3 Levels) - Made much harder
enemyStats IronbackMinotaur = (1200, 25, 60, 15, 150, 2.0)  -- Tank boss - doubled HP, more armor, faster attacks
enemyStats FireDrake = (1500, 20, 70, 250, 200, 1.8)       -- Ranged fire AoE boss - doubled HP, more damage
enemyStats LichKingArcthros = (1800, 30, 55, 220, 180, 2.5) -- Summoner + debuffer - tripled HP, high armor

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

towerStats :: TowerType -> (Float, Float, Float)
towerStats ArrowTower = (260, 30, 0.6)      -- Medium range, fast firing
towerStats CatapultTower = (380, 70, 1.8)     -- Long range, AoE
towerStats CrossbowTower = (400, 90, 1.2)    -- Very long range, precision
towerStats FireTower = (280, 20, 1.0)        -- Medium range, DoT
towerStats TeslaTower = (300, 40, 1.2)       -- Medium range, chain lightning
towerStats BallistaTower = (350, 110, 1.6)   -- Long range, piercing
towerStats PoisonTower = (270, 15, 1.0)       -- Medium range, debuff
towerStats BombardTower = (360, 130, 2.5)     -- Long range, AoE burst

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
gateMaxHP = 3000

gateUpgradeBaseCost :: Int
gateUpgradeBaseCost = 300  -- Base cost for upgrading gate

gateHPPerLevel :: Float
gateHPPerLevel = 500  -- HP increase per level

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
startingGold = 500


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