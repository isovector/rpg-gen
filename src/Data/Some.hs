{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( module Exports
    , Some
    , some
    , constrain
    , specify
    , specifys
    ) where

import Preface

import Control.Monad.IO.Class
import Math.Probable

import qualified Math.Probable as Exports hiding (RandT (..))
import qualified System.Random.MWC as MWC

type Some a = RandT IO a

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

some :: MonadIO m => Some a -> m a
some = let sysRandom = MWC.createSystemRandom
        in liftIO . (sysRandom >>=) . runRandT

