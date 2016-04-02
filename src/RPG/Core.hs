{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
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
    , tagL
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
    , propAddr
    , ActorId (..)
    , Actor (..)
    , hp
    , mp
    , team
    , Team
    ) where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Default
import Data.Function (on)
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
    { _hp :: Int
    -- , maxHp :: Int
    , _mp :: Int
    -- , maxMp :: Int
    , _team :: Team
    } deriving (Show, Eq)
$(makeLenses ''Actor)

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    , _propKey      :: Maybe Int
    , _interaction  :: Maybe Interaction
    , _propAddr     :: Maybe (Address (Prop' Tag))
    , _actor        :: Maybe Actor
    }
$(makeLenses ''Tag)

instance Eq Tag where
    t1 == t2 = let f g = on (==) g t1 t2
                in all id [ f _hasCollision
                          , f _isFloor
                          , f _propKey
                          , f _actor
                          ]

tagL :: Lens' Prop Tag
tagL = lens getTag $ flip tag

type Prop = Prop' Tag

instance Default Tag where
    def = Tag False False Nothing Nothing Nothing Nothing

hasInteraction :: Tag -> Bool
hasInteraction = isJust . _interaction

hasActor :: Tag -> Bool
hasActor = isJust . _actor

