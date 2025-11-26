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
fortWidth = 400
fortHeight = 600

fortLeft, fortRight, fortTop, fortBottom :: Float
fortLeft = fortCenterX - fortWidth / 2
fortRight = fortCenterX + fortWidth / 2
fortTop = fortHeight / 2
fortBottom = -fortHeight / 2

gateX :: Float
gateX = fortLeft

gateY :: Float
gateY = 0

gateWidth :: Float
gateWidth = 80

-- ============================================================================
-- Castle Position
-- ============================================================================

castleX, castleY :: Float
castleX = fortCenterX + 100
castleY = 0

castleSize :: Float
castleSize = 120  -- Larger base size for castle

-- ============================================================================
-- Wave & Level Constants
-- ============================================================================

wavesPerLevel :: Int
wavesPerLevel = 3

buildPhaseTime :: Float
buildPhaseTime = 30.0

bossPhaseTime :: Float
bossPhaseTime = 60.0

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
-- Normal Enemies (7 Total)
enemyStats GruntRaider = (60, 0, 72, 8, 40, 1.5)      -- Weak melee rusher (reduced range from 20 to 8)
enemyStats BruteCrusher = (150, 5, 40, 10, 70, 2.5)    -- Slow high-HP tank (reduced range from 30 to 10)
enemyStats Direwolf = (40, 0, 140, 6, 25, 1.0)        -- Ultra-fast low HP (reduced range from 15 to 6)
enemyStats Shieldbearer = (120, 8, 60, 8, 40, 2.0)     -- Projectile-resistant tank (reduced range from 25 to 8)
enemyStats Pyromancer = (80, 2, 55, 200, 15, 2.0)      -- Ranged caster (burns towers) - keep range
enemyStats Necromancer = (100, 3, 50, 180, 20, 2.5)    -- Summoner - keep range
enemyStats BoulderRamCrew = (250, 10, 50, 12, 150, 3.0) -- Anti-castle siege ram (reduced range from 30 to 12)
-- Bosses (Every 3 Levels)
enemyStats IronbackMinotaur = (600, 15, 55, 15, 100, 2.5)  -- Tank boss (reduced range from 35 to 15)
enemyStats FireDrake = (750, 12, 65, 250, 150, 2.0)        -- Ranged fire AoE boss - keep range
enemyStats LichKingArcthros = (900, 18, 50, 220, 120, 3.0) -- Summoner + debuffer - keep range

-- (hp, armor, speed, attackRange, damage, attackCooldown)

enemyGoldValue :: UnitType -> Int
-- Normal Units
enemyGoldValue GruntRaider = 10      -- Basic Enemy
enemyGoldValue BruteCrusher = 20     -- Tank Enemy
enemyGoldValue Direwolf = 12         -- Fast Runner
enemyGoldValue Shieldbearer = 18     -- Armored Unit
enemyGoldValue Pyromancer = 25       -- Ranged Caster
enemyGoldValue Necromancer = 30      -- Summoner
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