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
    , liftIO
    , module Control.Lens
    , module Control.Monad
    , module Data.Some
    , module Game.Sequoia
    , Key (..)
    , PropId (..)
    ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Data.Default
import Data.IORef (IORef (..), readIORef, writeIORef)
import Data.Some hiding (Event (..), never)
import Game.Sequoia
import Game.Sequoia.Keyboard

type Prop = Prop' Tag
type Has t r = Member (Reader t) r
newtype PropId = PropId Int deriving (Eq, Show, Ord)

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    , _propKey      :: Maybe PropId
    , _box          :: Maybe ((Prop -> Prop) -> IO ())
    , _interaction  :: Maybe (Now ())
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

