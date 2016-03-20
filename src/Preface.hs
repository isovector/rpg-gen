{-# LANGUAGE TemplateHaskell #-}
module Preface
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Game.Sequoia
    , Tag ()
    , Prop
    , Interaction (..)
    , hasCollision
    , ident
    , interaction
    , hasInteraction
    ) where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Default
import Data.Maybe (isJust)
import Game.Sequoia

data Interaction = Teleport Int Pos
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

