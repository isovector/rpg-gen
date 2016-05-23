module RPG.Scene
    ( Loc (..)
    , newSceneGraph
    ) where

import Control.Arrow
import Control.Monad (join, liftM2)
import Data.Map (Map)
import Data.Maybe (fromJust)
import RPG.Core
import Game.Sequoia.Utils
import qualified Data.Map as M

type SceneGraph = Map Loc (B [Prop])

newtype Loc = Loc Int
    deriving (Eq, Show, Ord)

newSceneGraph :: Now ( B [Prop]
                     , Loc -> B [Prop] -> IO ()
                     , Loc -> IO ()
                     )
newSceneGraph = do
    (graph, addScene) <- newCollection . M.singleton (Loc $ -1) $ pure []
    (loc, setLoc)     <- scanle const (Loc $ -1)
    return ( join . fmap fromJust $ loc >>= graph
           , addScene
           , setLoc
           )

