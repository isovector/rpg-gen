{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
module RPG.Logic.Combat.Types
    ( makeActor
    , sword
    , partitionActors
    ) where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import Data.Function (on)
import Data.Maybe (fromJust)
import RPG.Core
import RPG.Logic.QuickTime
import qualified Data.Map as M

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
sword dmg = Weapon 30 id (on (/=) _team) $ \params ->
    machine () $ do
        lift . forM_ (targeted params) $ \target -> do
            mail (address target) . over (_Just'.actor.hp)
                                  $ subtract dmg
        finish []

makeActor :: Address (Maybe Prop) -> Actor -> Signal ()
makeActor addr a = mail addr $ (_Just'.tagL.propActor .~ Just a)
                             . (_Just'.tagL.propAddr  .~ Just (addr))

_Just' :: Lens' (Maybe a) a
_Just' = lens fromJust (const Just)

