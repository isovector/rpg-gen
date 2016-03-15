{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Some
import qualified Game.World as W

import Control.Lens
import Control.Lens.TH (makeLenses)

data Person = Person Int    -- ^ age
                     Double -- ^ weight (kgs)
                     Double -- ^ salary (e.g euros)
    deriving (Eq, Show)

data World = World
    { _age :: Int
    , _inhab :: Person
    } deriving (Eq, Show)
makeLenses ''World

world :: Some World
world =
    World <$> uniformIn (0, 3000)
          <*> person

world2000 :: Some World
world2000 = specify age world 2000

worldYoung :: Some World
worldYoung = constrain age world $ uniformIn (0, 500)

worldOld :: Some World
worldOld = specifys age world (*10)

person :: Some Person
person =
    Person <$> uniformIn (1, 100)
           <*> uniformIn (20, 120)
           <*> uniformIn (500, 10000)

main :: IO ()
main = some worldOld >>= print

