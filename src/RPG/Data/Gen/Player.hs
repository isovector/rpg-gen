{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module RPG.Data.Gen.Player
    ( playerGen
    ) where

import Game.Sequoia.Color
import RPG.Core

playerGen :: Some Prop
playerGen = do
    color <- rgb <$> uniform 0 1
                 <*> uniform 0 1
                 <*> uniform 0 1
    return . filled color $ rect origin 20 20

