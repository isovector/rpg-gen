{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Core
    ( module Game.Sequoia
    , module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Control.Monad.IfElse
    , module Data.Some
    , Map
    , rgb
    , rgba
    , Prop
    , tagL
    , headL
    , Interaction (..)
    , Loc (..)
    , Tag ()
    , hasCollision
    , isFloor
    , propKey
    , interaction
    , hasInteraction
    , hasActor
    , actor
    , propActor
    , propAddr
    , Actor (..)
    , hp
    , mp
    , team
    , weapon
    , Team
    , Target (..)
    , AttackParams (..)
    , Attack
    , QuickTime
    , Weapon (..)
    , Machine (..)
    , machine
    ) where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.IfElse (whenM, untilM, return')
import Control.Monad.State hiding (state)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Some
import Game.Sequoia
import Game.Sequoia.Color (rgb, rgba)
import RPG.Internal

$(makeLenses ''Actor)
$(makeLenses ''Tag)

tagL :: Lens' Prop Tag
tagL = lens getTag $ flip tag

headL :: Lens' [a] a
headL = lens head (\as a -> a : tail as)

actor :: Setter' Prop Actor
actor = tagL.propActor._Just

hasInteraction :: Tag -> Bool
hasInteraction = isJust . _interaction

hasActor :: Tag -> Bool
hasActor = isJust . _propActor

machine :: s -> StateT s Signal (Bool, a) -> Machine a
machine s t = Machine run t s
  where
    continuing True = Just
    continuing _    = const Nothing
    run = do
        ((r, a'), s') <- runStateT t s
        return (continuing r $ machine s' t, a')

