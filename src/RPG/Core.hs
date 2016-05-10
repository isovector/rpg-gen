{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module RPG.Core
    ( Tag (..)
    , hasCollision
    , isFloor
    , propKey
    , box
    , interaction
    , Has
    , Eff
    , Prop
    , ask
    , def
    , lift
    , module Game.Sequoia
    , module Data.Some
    , module Control.Lens
    , module Control.Monad
    , Key (..)
    ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Monad
import Control.Lens
import Data.Default
import Data.Some hiding (Event (..), never)
import Game.Sequoia
import Game.Sequoia.Keyboard

type Prop = Prop' Tag
type Has t r = Member (Reader t) r

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    , _propKey      :: Maybe Int
    , _box          :: Maybe ((Prop -> Prop) -> IO ())
    , _interaction  :: Maybe (IO ())
    }
$(makeLenses ''Tag)

instance Show Tag where
    show Tag{..} = concat [ "Tag "
                          , show _hasCollision
                          , show _isFloor
                          , show _propKey
                          ]

instance Default Tag where
    def = Tag False False Nothing Nothing Nothing

