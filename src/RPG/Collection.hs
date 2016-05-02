module RPG.Collection
    ( scanle
    , newCollection
    ) where

import Control.Arrow
import Control.FRPNow.EvStream
import Data.Map (Map)
import RPG.Core
import Game.Sequoia.Utils
import qualified Data.Map as M

scanle :: (a -> b -> b)
       -> b
       -> N (B b, a -> IO ())
scanle f start = do
    (es, mb) <- callbackStream
    folded   <- sample $ scanlEv (flip f) start es
    b        <- sample $ fromChanges start folded
    return (b, mb)

newCollection :: Ord k
              => Map k v
              -> N ( k -> B (Maybe v)
                   , k -> v -> IO ()
                   )
newCollection start = do
    (b, mb) <- scanle (uncurry M.insert) start
    return ((<$> b) . M.lookup, curry mb)

