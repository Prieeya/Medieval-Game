-- Constants.hs - Game constants

module Constants where

import Types

windowWidth, windowHeight :: Int
windowWidth = 1000
windowHeight = 700

cellSize :: Float
cellSize = 35

gridW, gridH :: Int
gridW = windowWidth `div` round cellSize
gridH = windowHeight `div` round cellSize

towerCost :: TowerType -> Int
towerCost Arrow = 50
towerCost Cannon = 100
towerCost Ice = 75
towerCost Lightning = 150

baseTowerRange :: TowerType -> Float
baseTowerRange Arrow = 140
baseTowerRange Cannon = 110
baseTowerRange Ice = 170
baseTowerRange Lightning = 200

towerDamage :: TowerType -> Int
towerDamage Arrow = 15
towerDamage Cannon = 45
towerDamage Ice = 8
towerDamage Lightning = 30

towerCooldownTime :: TowerType -> Float
towerCooldownTime Arrow = 0.4
towerCooldownTime Cannon = 1.8
towerCooldownTime Ice = 0.6
towerCooldownTime Lightning = 2.5

enemyStats :: EnemyType -> (Int, Float, Int)
enemyStats BasicEnemy = (30, 35, 0)
enemyStats FastEnemy = (20, 60, 0)
enemyStats TankEnemy = (100, 20, 5)
enemyStats BossEnemy = (200, 25, 10)

enemyReward :: EnemyType -> Int
enemyReward BasicEnemy = 20
enemyReward FastEnemy = 30
enemyReward TankEnemy = 50
enemyReward BossEnemy = 100