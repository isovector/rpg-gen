{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Some
import Game.World
import Game.WorldGen

import Control.Lens
import Control.Lens.TH (makeLenses)

data Person = Person Int    -- ^ age
                     Double -- ^ weight (kgs)
                     Double -- ^ salary (e.g euros)
    deriving (Eq, Show)

world :: IO World
world = fmap fst . runWorldGenT $ do
    loc1 <- newLocation
    loc2 <- newLocation
    loc3 <- newLocation
    link loc1 loc2
    link loc2 loc3

person :: Some Person
person =
    Person <$> uniformIn (1, 100)
           <*> uniformIn (20, 120)
           <*> uniformIn (500, 10000)

main :: IO ()
main = some person >>= print

