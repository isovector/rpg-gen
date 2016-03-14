{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Some

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
worldOld =  specifies age world (*10)

person :: Some Person
person =
    Person <$> uniformIn (1, 100)
           <*> uniformIn (2, 130)
           <*> uniformIn (500, 10000)

main :: IO ()
main = pick worldOld >>= print

