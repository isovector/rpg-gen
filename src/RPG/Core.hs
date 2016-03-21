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
    , Loc
    , Tag ()
    , hasCollision
    , propKey
    , interaction
    , hasInteraction
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
    deriving (Eq, Show, Ord, Num)

type Prop = Prop' Tag

data Interaction = Teleport Loc Int
    deriving (Eq, Show)

data Tag = Tag
    { _hasCollision :: Bool
    , _propKey      :: Maybe Int
    , _interaction  :: Maybe Interaction
    }
    deriving (Eq, Show)
$(makeLenses ''Tag)

instance Default Tag where
    def = Tag False Nothing Nothing

hasInteraction :: Tag -> Bool
hasInteraction = isJust . _interaction

