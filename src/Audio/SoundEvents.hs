module Audio.SoundEvents where

import Types (SoundEvent(..), UnitType(..), TowerType(..), ProjectileType(..))

-- ============================================================================
-- Sound File Mapping
-- ============================================================================

-- Map sound events to file paths
soundEventToFile :: SoundEvent -> FilePath
soundEventToFile event = "assets/sounds/" ++ case event of
  -- Tower Sounds
  SoundTowerFire ArrowTower -> "tower_arrow_fire.wav"
  SoundTowerFire CatapultTower -> "tower_catapult_fire.wav"
  SoundTowerFire CrossbowTower -> "tower_crossbow_fire.wav"
  SoundTowerFire FireTower -> "tower_fire_fire.wav"
  SoundTowerFire TeslaTower -> "tower_tesla_fire.wav"
  SoundTowerFire BallistaTower -> "tower_ballista_fire.wav"
  SoundTowerFire PoisonTower -> "tower_poison_fire.wav"
  SoundTowerFire BombardTower -> "tower_bombard_fire.wav"
  SoundTowerBuild -> "build_tower.wav"
  SoundTowerSell -> "sell_tower.wav"
  SoundTowerUpgrade -> "upgrade_tower.wav"
  SoundTowerDestroyed -> "structure_destroyed.wav"
  
  -- Combat Sounds
  SoundProjectileHit Arrow -> "hit_arrow.wav"
  SoundProjectileHit BallistaBolt -> "hit_heavy.wav"
  SoundProjectileHit Fireball -> "hit_fire.wav"
  SoundProjectileHit IceShard -> "hit_ice.wav"
  SoundProjectileHit LightningBolt -> "hit_lightning.wav"
  SoundProjectileHit BarrageShot -> "hit_arrow.wav"
  SoundProjectileHit CatapultRock -> "hit_rock.wav"
  SoundEnemyHit _ -> "enemy_hit.wav"
  SoundEnemyDeath _ -> "enemy_death.wav"
  SoundEnemyAttack _ -> "enemy_attack.wav"
  SoundTrapTriggered -> "trap_trigger.wav"
  
  -- Fort Sounds
  SoundGateHit -> "gate_hit.wav"
  SoundGateDestroyed -> "gate_destroyed.wav"
  SoundGateRepaired -> "repair.wav"
  SoundWallHit -> "wall_hit.wav"
  SoundCastleHit -> "castle_hit.wav"
  
  -- UI/Game Sounds
  SoundWaveStart -> "wave_start.wav"
  SoundWaveComplete -> "wave_complete.wav"
  SoundLevelComplete -> "level_complete.wav"
  SoundGameOver -> "game_over.wav"
  SoundVictory -> "victory.wav"
  SoundButtonHover -> "ui_hover.wav"
  SoundButtonClick -> "ui_click.wav"
  SoundError -> "ui_error.wav"
  SoundUpgrade -> "upgrade_unlock.wav"