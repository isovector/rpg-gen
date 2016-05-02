module RPG.Collection
    ( foldingE
    , newCollection
    ) where

import Control.Arrow
import Control.FRPNow.EvStream
import Data.Map (Map)
import RPG.Core
import Game.Sequoia.Utils
import qualified Data.Map as M

-- TODO(sandy): this might be scanlEv
foldingE :: (a -> b -> b)
         -> b
         -> N (B b, a -> IO ())
foldingE f start = do
    (es, mb) <- callbackStream
    b <- loop es start
    return (b, mb)
  where
    loop es val = do
        val' <- (sample $ nextAll es)
                >>= return . fmap (foldr f val)
        e <- planNow $ loop es <$> val'
        return $ step val e

newCollection :: Ord k
              => Map k v
              -> N ( k -> B (Maybe v)
                   , k -> v -> IO ()
                   )
newCollection start = do
    (b, mb) <- foldingE (uncurry M.insert) start
    return ((<$> b) . M.lookup, curry mb)

