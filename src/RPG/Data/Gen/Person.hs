{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module RPG.Data.Gen.Person
    ( module RPG.Data.Person
    , personGen
    , personDraw
    ) where

import RPG.Core
import RPG.Data.Gen.Utils
import RPG.Data.Person

personGen :: Some r => Eff r Person
personGen = Person
        <$> anyColorGen
        <*> anyColorGen
        <*> enumGen
        <*> enumGen
        <*> uniform 5 15

shapeDraw :: NpcShape -> Double -> Shape
shapeDraw Square   size = rect origin (size * 2) (size * 2)
shapeDraw Circle   size = circle origin size
shapeDraw Triangle size = polygon origin [ mkRel 0 $ -size
                                         , mkRel size $ size
                                         , mkRel (-size) size
                                         ]

personDraw :: Person -> Prop
personDraw Person{..} = styled skinColor (defaultLine {lineColor = hairColor})
                      $ shapeDraw perShape perSize

