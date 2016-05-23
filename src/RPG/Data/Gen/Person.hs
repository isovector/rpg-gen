{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.Gen.Person
    ( module RPG.Data.Person
    , personGen
    , personBuild
    ) where

import Game.Sequoia.Color
import Game.Sequoia.Stanza
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

discussionGen :: ( Some r
                 , Has (Prop -> Time -> IO ()) r
                 )
              => Temperament
              -> Eff r (Now ())
discussionGen t = do
    (addTmpObj :: Prop -> Time -> IO ()) <- ask
    msg <- uniformly $ case t of
                Happy -> [ "hello!"
                         , "i love life!"
                         , "what a great day"
                         , ":)"
                         ]
                Sad   -> [ "bah humbug"
                         , "grumble"
                         , "everything is terrible"
                         , ":("
                         ]
    return . sync $ do
        addTmpObj ( StanzaProp
                  . monospace
                  . aligned Centered
                  . color black
                  . height 12
                  $ toStanza msg) 2

discussionProp :: ( Some r
                  , Has (Prop -> Time -> IO ()) r
                  )
               => Temperament
               -> Eff r Prop
discussionProp t = do
    discussion <- discussionGen t
    return . tags (interaction .~ Just discussion)
           . traced yellow
           $ rect origin 40 40

personBuild :: ( Some r
               , Has (Prop -> Time -> IO ()) r
               )
            => Person
            -> Eff r [Prop]
personBuild p@Person{..} = (: [personDraw p]) <$> discussionProp temperament

personDraw :: Person -> Prop
personDraw Person{..} = tags (hasCollision .~ True)
                      . styled skinColor (defaultLine {lineColor = hairColor})
                      $ shapeDraw perShape perSize

