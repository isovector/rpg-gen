module RPG.Data.Gen.Player
    ( playerGen
    ) where

import RPG.Core

playerGen :: Some (Maybe Prop)
playerGen = do
    color <- rgb <$> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
    return . Just . filled color $ rect origin 20 20

