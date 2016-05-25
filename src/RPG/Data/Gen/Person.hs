{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import RPG.Scene

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
shapeDraw Triangle size = polygon origin [ rel 0 $ -size
                                         , rel size $ size
                                         , rel (-size) size
                                         ]


discussionGen :: ( Some r
                 , Has (B Prop -> Time -> IO ()) r
                 , Has (Loc -> PropId -> B (Maybe Prop)) r
                 )
              => Temperament
              -> Loc
              -> PropId
              -> Eff r (Now ())
discussionGen t loc key = do
    (addTmpObj :: B Prop -> Time -> IO ())         <- ask
    (findProp  :: Loc -> PropId -> B (Maybe Prop)) <- ask
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
    let prop = Leaf
             . StanzaPiece def
             . monospace
             . aligned Centered
             . color black
             . height 12
             $ toStanza msg
        tracer = findProp loc key

    return . sync . flip addTmpObj 2 $ do
        tracer >>= pure . \case
            Just traced -> teleport ( plusDir (center traced)
                                    . rel 0 $ -30
                                    ) prop
            Nothing     -> prop


discussionProp :: ( Some r
                  , Has (B Prop -> Time -> IO ()) r
                  , Has (Loc -> PropId -> B (Maybe Prop)) r
                  )
               => Temperament
               -> Loc
               -> PropId
               -> Eff r Prop
discussionProp t loc key = do
    discussion <- discussionGen t loc key
    return . tagging (interaction .~ Just discussion)
           . traced yellow
           $ rect origin 40 40


personBuild :: ( Some r
               , Has (B Prop -> Time -> IO ()) r
               , Has (Loc -> PropId -> B (Maybe Prop)) r
               )
            => Loc
            -> Person
            -> Eff r Prop
personBuild loc p@Person{..} = do
    idkey <- PropId <$> int
    let prop = tagging (propKey .~ Just idkey) $ personDraw p
    discussion <- discussionProp temperament loc idkey
    return $ group [discussion, prop]


personDraw :: Person -> Prop
personDraw Person{..} = tagging (hasCollision .~ True)
                      . styled skinColor (defaultLine {lineColor = hairColor})
                      $ shapeDraw perShape perSize

