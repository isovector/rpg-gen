{-# LANGUAGE Rank2Types #-}
module RPG.Logic.Utils
    ( newX
    , addX'
    , addX
    , getCurX
    , bounded
    , torus
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


-- Smart Lenses
bounded :: Lens' s [a] -> Lens' s Int -> Lens' s Int
bounded c i = lens (view i) $ \s v ->
    s & i .~ (max 0 $ min (subtract 1 . length $ view c s) v)

torus :: Lens' s [a] -> Lens' s Int -> Lens' s Int
torus c i = lens (view i) $ \s v ->
    s & i .~ let len = length $ view c s
              in mod (len + mod v len) len
