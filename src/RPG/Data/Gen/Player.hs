module RPG.Data.Gen.Player
    ( playerGen
    ) where

import Game.Sequoia.Color
import RPG.Core

playerGen :: Some Prop
playerGen = do
    color <- rgb <$> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
    return . filled color $ rect origin 20 20

