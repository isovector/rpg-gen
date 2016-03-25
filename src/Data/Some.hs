{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( module Exports
    , Some
    , pick
    , picking
    , constrain
    , specify
    , specifys
    , uniform
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Math.Probable hiding (uniform)
import System.IO.Unsafe (unsafePerformIO)
import qualified Math.Probable as Exports hiding (RandT (..), uniform)
import qualified System.Random.MWC as MWC

type Some a = RandT IO a

uniform :: Double -> Double -> Some Double
uniform = curry uniformIn

constrain :: Lens a a v v
          -> Some a
          -> Some v
          -> Some a
constrain l c v = set l <$> v <*> c

specify :: Lens a a v v
        -> Some a
        -> v
        -> Some a
specify l c v = set l v <$> c

specifys :: Lens a a v v
         -> Some a
         -> (v -> v)
         -> Some a
specifys l c v = over l v <$> c

pick :: MonadIO m => Some a -> m a
pick = let sysRandom = MWC.createSystemRandom
        in liftIO . (sysRandom >>=) . runRandT

picking :: Some a -> (a -> b) -> b
picking sa f = f . unsafePerformIO $ pick sa

