{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module RPG.Data.Gen.Utils
    ( colorGen
    , anyColorGen
    , colorFuzz
    , enumGen
    ) where

import RPG.Core
import Game.Sequoia.Color

type Range = (Double, Double)

anyColorGen :: Some r => Eff r Color
anyColorGen = colorGen (0, 1) (0, 1) (0, 1)

colorGen :: Some r => Range -> Range -> Range -> Eff r Color
colorGen r g b = rgb <$> uniformIn r <*> uniformIn g <*> uniformIn b

clamp :: Double -> Double -> Double -> Double
clamp l h v = max l $ min h v

colorFuzz :: Some r => Double -> Color -> Eff r Color
colorFuzz d (Color r g b a) = do
    dr <- uniformIn (-d, d)
    dg <- uniformIn (-d, d)
    db <- uniformIn (-d, d)
    return $ rgba (clampColor $ r + dr)
                  (clampColor $ g + dg)
                  (clampColor $ b + db)
                  a
  where
    clampColor = clamp 0 1

enumGen :: forall a r. (Bounded a, Enum a, Some r) => Eff r a
enumGen =
    let min = minBound :: a
        max = maxBound :: a
     in toEnum <$> uniform (fromEnum min) (fromEnum max)

