module Game.World where

import Preface

import Control.Monad.IO.Class
import Data.Graph
import Data.IORef
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, tagWith, proxy)
import System.IO.Unsafe (unsafePerformIO)

type LocKey = Tagged Loc Int

data World = World
    { worldGraph :: Graph
    , getLocation' :: Vertex -> (Loc, LocKey, [LocKey])
    , getVertex' :: LocKey -> Maybe Vertex
    }

data Loc = Loc
    { locKey :: LocKey
    } deriving Eq

locKeyRef :: IORef Int
locKeyRef = unsafePerformIO $ newIORef 0

nextLocKey :: MonadIO m => m LocKey
nextLocKey = liftIO $ do
    key <- readIORef locKeyRef
    modifyIORef locKeyRef (+1)
    return $ tagWith (Proxy :: Proxy Loc) key

newLocation :: MonadIO m => m Loc
newLocation = Loc <$> nextLocKey

