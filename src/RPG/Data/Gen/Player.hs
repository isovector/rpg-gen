module RPG.Data.Gen.Player
    ( Player (..)
    , playerGen
    ) where

import RPG.Core

data Player = Player
    { prop :: Prop
    , speed :: Double
    } deriving Eq

playerGen :: Some Player
playerGen = do
    color <- rgb <$> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
    let form = filled color $ rect origin 20 20
    return $ Player form 300

