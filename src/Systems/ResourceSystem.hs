module Systems.ResourceSystem where

import Types

-- ============================================================================
-- Resource Management
-- ============================================================================

spendGold :: Int -> Resources -> Resources
spendGold amount resources =
  resources
    { resGold = resGold resources - amount
    , resTotalSpent = resTotalSpent resources + amount
    }

earnGold :: Int -> Resources -> Resources
earnGold amount resources =
  resources
    { resGold = resGold resources + amount
    , resTotalEarned = resTotalEarned resources + amount
    }