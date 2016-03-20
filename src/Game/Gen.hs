module Game.Gen
    ( picking
    ) where

import Data.Some
import System.IO.Unsafe (unsafePerformIO)

picking :: Some a -> (a -> b) -> b
picking a f = f . unsafePerformIO $ pick a

