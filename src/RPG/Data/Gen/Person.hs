{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module RPG.Data.Gen.Person
    ( module RPG.Data.Person
    , personGen
    , personBuild
    ) where

import Game.Sequoia.Color
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

discussionGen :: Some r => Temperament -> Eff r (Now ())
discussionGen t = fmap (sync . putStrLn) . uniformly
                $ case t of
                    Happy -> ["hello!", "i love life!"]
                    Sad   -> ["bah humbug"]

discussionProp :: Some r => Temperament -> Eff r Prop
discussionProp t = do
    discussion <- discussionGen t
    return . tags (interaction .~ Just discussion)
           . traced yellow
           $ rect origin 40 40

personBuild :: Some r => Person -> Eff r [Prop]
personBuild p@Person{..} = (: [personDraw p]) <$> discussionProp temperament

personDraw :: Person -> Prop
personDraw Person{..} = tags (hasCollision .~ True)
                      . styled skinColor (defaultLine {lineColor = hairColor})
                      $ shapeDraw perShape perSize

