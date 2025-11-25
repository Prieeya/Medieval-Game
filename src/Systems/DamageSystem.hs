module Systems.DamageSystem where

import Types
import Constants
import qualified Data.Map.Strict as M

-- ============================================================================
-- Damage Application
-- ============================================================================

applyQueuedDamage :: World -> World
applyQueuedDamage = id

applyDamageToEnemy :: Float -> Enemy -> Enemy
applyDamageToEnemy damage enemy =
  let actualDamage = max 0 (damage - enemyArmor enemy)
      newHP = enemyHP enemy - actualDamage
  in enemy
    { enemyHP = newHP
    , enemyHitFlash = 0.2
    }

applyDamageToGate :: Float -> Gate -> Gate
applyDamageToGate damage gate =
  let newHP = max 0 (gateHP gate - damage)
      destroyed = newHP <= 0
  in gate
    { gateHP = newHP
    , gateDestroyed = destroyed
    }

applyDamageToCastle :: Float -> Castle -> Castle
applyDamageToCastle damage castle =
  let newHP = max 0 (castleHP castle - damage)
  in castle { castleHP = newHP }

applyDamageToWall :: Float -> WallSegment -> WallSegment
applyDamageToWall damage wall =
  let newHP = max 0 (wallHP wall - damage)
  in wall { wallHP = newHP }

applyDamageToTower :: Float -> Tower -> Tower
applyDamageToTower damage tower =
  let newHP = max 0 (towerHP tower - damage)
  in tower { towerHP = newHP }