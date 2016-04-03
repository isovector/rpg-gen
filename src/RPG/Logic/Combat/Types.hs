{-# LANGUAGE LambdaCase #-}
module RPG.Logic.Combat.Types
    ( Effects (..)
    , runEffects
    , makeActor
    , sword
    , partitionActors
    ) where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import Data.Function (on)
import RPG.Core
import RPG.Logic.QuickTime
import qualified Data.Map as M

data Effects = Effects
    { managedProps :: Signal [Prop]
    , stillRunning :: Signal Bool
    }

-- TODO(sandy): maybe split this up to run in Signal?
runEffects :: Int -> Effects -> QuickTime a [Prop]
runEffects s e = do
    running <- lift $ stillRunning e
    if running
       then lift $ managedProps e
       else setState s >> return []

partitionActors :: Prop -> [Prop] -> ([Target], [Prop])
partitionActors me ps = first (map toTarget)
                      $ partition (hasActor . getTag) ps
  where
    toTarget p =
        let a = maybe (error "not an actor") id
              . view propActor
              $ getTag p
            addr = maybe (error "doesn't have an addr") id
                 . view propAddr
                 $ getTag p
            pos = center me
            occluded = (1 <)
                     . length
                     . sweepLine ps pos
                     . posDif pos
                     $ center p
         in Target a (center p) addr occluded

sword :: Int -> Weapon [Prop]
sword dmg = Weapon 30 id (on (/=) _team) $ \params -> \case
    Just 0 -> do
        lift . forM_ (targeted params) $ \target ->
            mail (address target) . over (actor.hp)
                                  $ subtract dmg
        finish
        return []
    _ -> return []

makeActor :: Address Prop -> Actor -> Signal ()
makeActor addr a = mail addr $ (tagL.propActor .~ Just a)

