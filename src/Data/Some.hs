{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Data.Some
    ( runLift
    , Some
    , pick
    , picking
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

type Some a = forall r. SetMember Lift (Lift (RandT IO)) r => Eff r a

instance MonadIO (RandT IO) where
    liftIO = RandT . const

uniform :: MWC.Variate a => a -> a -> Some a
uniform = (lift . ) . curry MP.uniformIn

uniformIn :: MWC.Variate a => (a, a) -> Some a
uniformIn = uncurry uniform

uniformly :: SetMember Lift (Lift (RandT IO)) r => [Eff r a] -> Eff r a
uniformly = weighted . fmap (, 1)

int :: Some Int
int = lift MP.int

listOf :: Int -> Some a -> Some [a]
listOf n g = sequence . take n $ repeat g

weighted :: SetMember Lift (Lift (RandT IO)) r
         => [(Eff r a, Double)] -> Eff r a
-- TODO(sandy): this isn't weighted
weighted ws = do
    v <- (`mod` length ws) <$> int
    fst $ ws !! v

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

pick :: Eff (Lift (RandT IO) :> Void) a -> IO a
pick = let sysRandom = MWC.createSystemRandom
        in liftIO . (sysRandom >>=) . runRandT . runLift

picking :: Some a -> (a -> b) -> b
picking sa f = f . unsafePerformIO $ pick sa

