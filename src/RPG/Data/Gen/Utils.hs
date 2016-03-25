module RPG.Data.Gen.Utils
    ( colorGen
    , colorFuzz
    ) where

import RPG.Core
import Game.Sequoia.Color

type Range = (Double, Double)

colorGen :: Range -> Range -> Range -> Some Color
colorGen r g b = rgb <$> uniformIn r <*> uniformIn g <*> uniformIn b

clamp :: Double -> Double -> Double -> Double
clamp l h v = max l $ min h v

clampColor = clamp 0 1

colorFuzz :: Double -> Color -> Some Color
colorFuzz d (Color r g b a) = do
    dr <- uniformIn (-d, d)
    dg <- uniformIn (-d, d)
    db <- uniformIn (-d, d)
    return $ rgba (clampColor $ r + dr)
                  (clampColor $ g + dg)
                  (clampColor $ b + db)
                  a

