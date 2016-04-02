module RPG.Logic.Utils
    ( newX
    , addX'
    , addX
    , getCurX
    ) where

import Data.IORef
import RPG.Core
import qualified Data.Map as M
import qualified Data.Traversable as T

newX :: (k -> k) -> IORef k -> IO k
newX f ref = readIORef ref <* modifyIORef ref f

{-# NOINLINE addX' #-}
addX' :: Ord k => Address (Map k v) -> k -> v -> IO ()
addX' addr k v = mail' addr . mappend $ M.singleton k v

addX :: Ord k => Address (Map k v) -> k -> v -> Signal ()
addX addr k v = mail addr . mappend $ M.singleton k v

getCurX :: Ord k => Signal (Map k v) -> Signal k -> Signal v
getCurX s k = (M.!) <$> s <*> k

