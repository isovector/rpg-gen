module Game.Gen.Player
    ( Player (..)
    , playerGen
    ) where

import Preface

import Data.Some
import Game.Sequoia
import Game.Sequoia.Color (rgb)

data Player = Player
    { prop :: Prop
    , speed :: Double
    }

playerGen :: Some Player
playerGen = do
    color <- rgb <$> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
    let form = filled color $ rect origin 20 20
    return $ Player form 300

