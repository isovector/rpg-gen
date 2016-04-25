module RPG.Scene
    ( Loc (..)
    , newSceneGraph
    ) where

import Control.Arrow
import Control.Monad (join, liftM2)
import Data.Map (Map)
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
                     , Address (Loc -> Loc)
                     )
newSceneGraph startloc start = do
    (graph, graphAddr) <- foldmp (M.singleton startloc start) return
    (loc, locAddr)     <- foldmp startloc return
    traceChanges "loc" loc
    return ( currentScene graph loc
           , (curry $ addScene graphAddr) . showTrace
           , locAddr
           )
  where
      addScene addr = addr . mappend . uncurry M.singleton
      currentScene = (join .) . liftM2 (M.!)

