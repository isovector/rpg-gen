module RPG.Collection
    (
    ) where

import Control.Arrow
import Control.FRPNow.EvStream
import Data.Map (Map)
import RPG.Core
import Game.Sequoia.Utils
import qualified Data.Map as M

newCollection :: Ord k
              => Map k v
              -> N ( k -> B (Maybe v)
                   , k -> v -> IO ()
                   )
newCollection start = do
    (es, mb) <- callbackStream
    b <- loop es start
    return ((<$> b) . M.lookup, curry mb)
  where
    loop es val = do
        val' <- (sample $ nextAll es)
                >>= return . fmap (foldr (uncurry M.insert) val)
        e <- planNow $ loop es <$> val'
        return $ step val e

