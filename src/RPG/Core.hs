{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Core
    ( module Game.Sequoia
    , module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Data.Some
    , def
    , Map
    , rgb
    , rgba
    , Prop
    , Interaction (..)
    , Loc (..)
    , Tag ()
    , hasCollision
    , isFloor
    , propKey
    , interaction
    , hasInteraction
    , hasActor
    , actorId
    , ActorId (..)
    , Actor (..)
    , hp
    , mp
    , team
    , actorAddr
    , Team
    ) where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Default
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Some
import Game.Sequoia
import Game.Sequoia.Color (rgb, rgba)

newtype Loc = Loc Int
    deriving (Eq, Show, Ord)

data Interaction = Teleport Loc Int
    deriving (Eq, Show)

newtype ActorId = ActorId Int
    deriving (Eq, Show, Ord)

type Team = Int

data Actor = Actor
    { _actorAddr :: Address Actor
    , _hp :: Int
    -- , maxHp :: Int
    , _mp :: Int
    -- , maxMp :: Int
    , _team :: Team
    }
$(makeLenses ''Actor)

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    , _propKey      :: Maybe Int
    , _interaction  :: Maybe Interaction
    , _actorId      :: Maybe ActorId
    }
    deriving (Eq, Show)
$(makeLenses ''Tag)

type Prop = Prop' Tag

instance Default Tag where
    def = Tag False False Nothing Nothing Nothing

hasInteraction :: Tag -> Bool
hasInteraction = isJust . _interaction

hasActor :: Tag -> Bool
hasActor = isJust . _actorId

