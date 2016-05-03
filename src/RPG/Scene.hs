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

newSceneGraph :: Loc
              -> B [Prop]
              -> Now ( B [Prop]
                     , Loc -> B [Prop] -> IO ()
                     , Loc -> IO ()
                     )
newSceneGraph startloc start = do
    (graph, addScene) <- newCollection $ M.singleton startloc start
    (loc, setLoc)     <- scanle const startloc
    return ( join . fmap fromJust $ loc >>= graph
           , addScene
           , setLoc
           )

