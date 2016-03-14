{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Math.Probable

import Control.Lens
import Control.Lens.TH (makeLenses)

data Person = Person Int    -- ^ age
                     Double -- ^ weight (kgs)
                     Double -- ^ salary (e.g euros)
    deriving (Eq, Show)

data World = World { _age :: Int
                   , _inhab :: Person
                   } deriving (Eq, Show)
makeLenses ''World

type RLens r s = Lens s s r r

world :: RandT IO World
world =
    World <$> uniformIn (0, 3000)
          <*> person

world2000 :: RandT IO World
world2000 = specify age world 2000

worldYoung :: RandT IO World
worldYoung = constrain age world $ uniformIn (0, 500)

worldOld :: RandT IO World
worldOld =  specifys age world (*10)

constrain :: Monad m => RLens r a -> RandT m a -> RandT m r -> RandT m a
constrain l c v = set l <$> v <*> c

specify :: Monad m => RLens r a -> RandT m a -> r -> RandT m a
specify l c v = set l v <$> c

specifys :: Monad m => RLens r a -> RandT m a -> (r -> r) -> RandT m a
specifys l c v = over l v <$> c

person :: RandT IO Person
person =
    Person <$> uniformIn (1, 100)
           <*> uniformIn (2, 130)
           <*> uniformIn (500, 10000)

randomPersons :: Int -> IO [Person]
randomPersons n = mwc $ listOf n person

main :: IO ()
main = mwc worldOld >>= print

