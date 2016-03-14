{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( module Exports
    , Varying
    , Some
    , pick
    , constrain
    , specify
    , specifies
    ) where

import Control.Applicative
import Control.Monad
import Math.Probable
import Control.Monad.IO.Class

import Control.Lens

import qualified Math.Probable as Exports hiding (RandT (..))

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

-- TODO(sandy): make this work for arbitrary Varying m
pick :: MonadIO m => Varying IO a -> m a
pick = liftIO . mwc

