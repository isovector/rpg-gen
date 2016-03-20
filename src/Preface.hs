{-# LANGUAGE TemplateHaskell #-}
module Preface
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Data.Default
    , module Game.Sequoia
    , module Game.World
    , Map
    , Tag ()
    , Prop
    , Interaction (..)
    , hasCollision
    , ident
    , interaction
    , hasInteraction
    , findProp
    ) where

import Game.World

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Default
import Data.List (find)
import Data.Maybe (isJust)
import Game.Sequoia
import Data.Map (Map)

data Interaction = Teleport LocKey Int
    deriving (Eq, Show)

data Tag = Tag
    { _hasCollision :: Bool
    , _ident        :: Maybe Int
    , _interaction  :: Maybe Interaction
    }
    deriving (Eq, Show)
$(makeLenses ''Tag)

instance Default Tag where
    def = Tag False Nothing Nothing

type Prop = Prop' Tag

hasInteraction :: Tag -> Bool
hasInteraction = isJust . _interaction

findProp :: [Prop] -> Int -> Maybe Prop
findProp ps i = find (maybe False (== i) . _ident . getTag) ps

