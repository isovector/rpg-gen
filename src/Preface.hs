module Preface
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Game.Sequoia
    , Block (..)
    , Prop
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Game.Sequoia

data Block = Track | Wall deriving (Eq, Show)
type Prop = Prop' Block

