{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module RPG.Core
    ( Tag (..)
    , Has
    , Eff
    , Prop
    , ask
    , def
    , module Game.Sequoia
    , module Data.Some
    , Key (..)
    ) where

import Control.Eff
import Control.Eff.Reader.Lazy
import Data.Default
import Data.Some hiding (Event (..), never)
import Game.Sequoia
import Game.Sequoia.Keyboard

type Prop = Prop' Tag
type Has t r = Member (Reader t) r

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    }
    deriving (Eq, Show)

instance Default Tag where
    def = Tag False False

