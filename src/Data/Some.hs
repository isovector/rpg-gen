{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( runLift
    , Eff
    , Some
    , pick
    , constrain
    , specify
    , specifys
    , int
    , listOf
    , uniform
    , uniformIn
    , uniformly
    , weighted
    ) where

import Control.Eff
import Control.Eff.Lift
import Control.Lens
import Control.Monad.IO.Class
import Data.Void
import Math.Probable (RandT (..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Math.Probable as MP
import qualified System.Random.MWC as MWC

type Some r = SetMember Lift (Lift (RandT IO)) r

instance MonadIO (RandT IO) where
    liftIO = RandT . const

uniform :: Some r
        => MWC.Variate a => a -> a -> Eff r a
uniform = (lift . ) . curry MP.uniformIn

uniformIn :: Some r
          => MWC.Variate a => (a, a) -> Eff r a
uniformIn = uncurry uniform

uniformly :: Some r
          => [a] -> Eff r a
uniformly = weighted . fmap (, 1)

int :: Some r
    => Eff r Int
int = lift MP.int

listOf :: Some r
       => Int -> Eff r a -> Eff r [a]
listOf n g = sequence . take n $ repeat g

weighted :: Some r
         => [(a, Double)] -> Eff r a
-- TODO(sandy): this isn't weighted
weighted ws = do
    v <- (`mod` length ws) <$> int
    return . fst $ ws !! v

constrain :: Some r
          => Lens a a v v
          -> Eff r a
          -> Eff r v
          -> Eff r a
constrain l c v = set l <$> v <*> c

specify :: Some r
        => Lens a a v v
        -> Eff r a
        -> v
        -> Eff r a
specify l c v = set l v <$> c

specifys :: Some r
         => Lens a a v v
         -> Eff r a
         -> (v -> v)
         -> Eff r a
specifys l c v = over l v <$> c

pick :: Eff (Lift (RandT IO) :> Void) a -> IO a
pick = let sysRandom = MWC.createSystemRandom
        in liftIO . (sysRandom >>=) . runRandT . runLift

