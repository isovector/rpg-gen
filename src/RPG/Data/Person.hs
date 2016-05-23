module RPG.Data.Person
    ( Person (..)
    , Temperament (..)
    , NpcShape (..)
    ) where

import Game.Sequoia.Color

data Temperament = Happy | Sad deriving (Show, Eq, Enum, Bounded)

data NpcShape = Square | Circle | Triangle deriving (Show, Eq, Enum, Bounded)

data Person = Person
    { skinColor :: Color
    , hairColor :: Color
    , temperament :: Temperament
    , perShape :: NpcShape
    , perSize :: Double
    } deriving (Show, Eq)

