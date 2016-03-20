module Game.Gen
    ( pick
    , picking
    ) where

import Data.Some
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE picking #-}
picking :: Some a -> (a -> b) -> b
picking a f = f . unsafePerformIO $ pick a

