module Preface
    ( module Control.Applicative
    , module Control.Lens
    , module Control.Monad
    , module Game.Sequoia
    , Block (..)
    , Prop
    , isInteractive
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Game.Sequoia

data Block = Interactive Int
           | Wall
           deriving (Eq, Show)
type Prop = Prop' Block

isInteractive :: Block -> Bool
isInteractive (Interactive _) = True
isInteractive _ = False

