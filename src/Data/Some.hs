{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( module Exports
    , Varying
    , Some
    , some
    , constrain
    , specify
    , specifies
    ) where

import Control.Applicative hiding (some)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive (PrimBase)
import Math.Probable

import qualified Math.Probable as Exports hiding (RandT (..))
import qualified System.Random.MWC as MWC

type Varying m a = RandT m a
type Some a = Varying IO a

constrain :: Monad m
          => Lens a a v v
          -> Varying m a
          -> Varying m v
          -> Varying m a
constrain l c v = set l <$> v <*> c

specify :: Monad m
        => Lens a a v v
        -> Varying m a
        -> v
        -> Varying m a
specify l c v = set l v <$> c

specifies :: Monad m
          => Lens a a v v
          -> Varying m a
          -> (v -> v)
          -> Varying m a
specifies l c v = over l v <$> c

some :: (MonadIO m, PrimBase m) => Varying m a -> m a
some = liftIO . MWC.withSystemRandom . runRandT

