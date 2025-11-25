module Assets (ensureAssets, assetPath, getSpritePath, getAnimatedSpritePath) where

import Codec.Picture
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

assetPath :: FilePath
assetPath = "assets"

imagesPath :: FilePath
imagesPath = assetPath </> "images"

-- Get path to a static sprite (legacy support)
getSpritePath :: String -> FilePath
getSpritePath name = assetPath </> name

-- Get path to an animated sprite frame
-- Format: assets/images/<category>/<name>/<animation>/frame_XX.png
getAnimatedSpritePath :: String -> String -> String -> Int -> FilePath
getAnimatedSpritePath category name animation frameNum =
  let frameName = "frame_" ++ (if frameNum < 10 then "0" else "") ++ show frameNum ++ ".png"
  in imagesPath </> category </> name </> animation </> frameName

-- Ensure assets directory exists
ensureAssets :: IO ()
ensureAssets = do
  createDirectoryIfMissing True assetPath
  createDirectoryIfMissing True imagesPath
  -- Create subdirectories for organized sprite storage
  mapM_ (createDirectoryIfMissing True) 
    [ imagesPath </> "towers"
    , imagesPath </> "traps"
    , imagesPath </> "enemies"
    , imagesPath </> "bosses"
    , imagesPath </> "environment"
    , imagesPath </> "projectiles"
    ]
  -- Still create fallback sprites if new ones don't exist
  mapM_ (writeIfMissing assetPath) assetFiles

writeIfMissing :: FilePath -> (FilePath, (Int, Int), (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8)) -> IO ()
writeIfMissing dir (name, (w,h), painter) = do
  let fp = dir </> name
  exists <- doesFileExist fp
  if exists
    then return ()
    else do
      let img = generateImage (\nx ny -> painter (PixelRGBA8 0 0 0 0) (PixelRGBA8 (fromIntegral ((nx * 37) `mod` 255)) (fromIntegral ((ny * 61) `mod` 255)) 128 255)) w h
      writePng fp img

-- Legacy asset files (fallback if new sprites don't exist)
assetFiles :: [(FilePath, (Int, Int), (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8))]
assetFiles =
  [ ("enemy_grunt.png", (32,32), simplePainter (PixelRGBA8 180 60 60 255))
  , ("enemy_shieldbearer.png", (36,36), simplePainter (PixelRGBA8 150 150 160 255))
  , ("enemy_eliteknight.png", (40,40), simplePainter (PixelRGBA8 180 170 140 255))
  , ("enemy_runner.png", (28,28), simplePainter (PixelRGBA8 200 120 80 255))
  , ("enemy_raiderrogue.png", (32,32), simplePainter (PixelRGBA8 170 90 120 255))
  , ("enemy_archer.png", (32,32), simplePainter (PixelRGBA8 120 180 100 255))
  , ("enemy_crossbowman.png", (34,34), simplePainter (PixelRGBA8 140 140 120 255))
  , ("enemy_tank.png", (44,44), simplePainter (PixelRGBA8 120 120 120 255))
  , ("enemy_ogre.png", (46,46), simplePainter (PixelRGBA8 150 100 80 255))
  , ("enemy_catapult.png", (40,28), simplePainter (PixelRGBA8 120 100 100 255))
  , ("enemy_ram.png", (40,30), simplePainter (PixelRGBA8 130 90 70 255))
  , ("enemy_warlord.png", (56,56), simplePainter (PixelRGBA8 200 80 80 255))
  , ("enemy_siegecommander.png", (60,60), simplePainter (PixelRGBA8 190 150 90 255))
  , ("enemy_gianttroll.png", (64,64), simplePainter (PixelRGBA8 140 90 60 255))

  , ("tower_archer.png", (32,32), simplePainter (PixelRGBA8 100 140 80 255))
  , ("tower_ballista.png", (36,36), simplePainter (PixelRGBA8 140 120 100 255))
  , ("tower_fire.png", (34,34), simplePainter (PixelRGBA8 200 120 80 255))
  , ("tower_frost.png", (34,34), simplePainter (PixelRGBA8 120 180 220 255))
  , ("tower_lightning.png", (34,34), simplePainter (PixelRGBA8 220 220 120 255))
  , ("tower_barrage.png", (34,34), simplePainter (PixelRGBA8 160 120 160 255))
  , ("tower_guardian.png", (38,38), simplePainter (PixelRGBA8 200 200 180 255))

  , ("trap_spikepit.png", (28,28), simplePainter (PixelRGBA8 100 100 100 255))
  , ("trap_tarpit.png", (28,28), simplePainter (PixelRGBA8 60 40 30 255))
  , ("trap_firepot.png", (28,28), simplePainter (PixelRGBA8 220 120 40 255))
  , ("trap_explosiverune.png", (30,30), simplePainter (PixelRGBA8 200 80 80 255))
  , ("trap_caltrop.png", (28,28), simplePainter (PixelRGBA8 140 140 140 255))

  , ("proj_arrow.png", (12,6), simplePainter (PixelRGBA8 200 200 160 255))
  , ("proj_ballistabolt.png", (14,8), simplePainter (PixelRGBA8 160 160 160 255))
  , ("proj_fireball.png", (14,14), simplePainter (PixelRGBA8 255 120 40 255))
  , ("proj_iceshard.png", (10,14), simplePainter (PixelRGBA8 120 200 255 255))
  , ("proj_lightning.png", (12,12), simplePainter (PixelRGBA8 255 255 120 255))
  , ("proj_barrage.png", (10,10), simplePainter (PixelRGBA8 180 160 140 255))
  , ("proj_catapultrock.png", (18,18), simplePainter (PixelRGBA8 120 120 100 255))

  , ("fort_gate.png", (64,64), simplePainter (PixelRGBA8 120 80 40 255))
  , ("castle.png", (80,80), simplePainter (PixelRGBA8 160 140 100 255))
  ]

-- Very small helper painter that produces a colored rounded-ish sprite with simple shading
simplePainter :: PixelRGBA8 -> (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8)
simplePainter (PixelRGBA8 r g b a) _ _ = PixelRGBA8 r g b a
