module RPG.Scene
    ( Loc (..)
    , newSceneGraph
    ) where

import Control.Arrow
import Control.Monad (join, liftM2)
import Data.Map (Map)
import RPG.Core
import qualified Data.Map as M

type SceneGraph = Map Loc (B [Prop])

newtype Loc = Loc Int
    deriving (Eq, Show, Ord)

newSceneGraph :: Loc
              -> B [Prop]
              -> Now ( B [Prop]
                     , Address (Loc, B [Prop])
                     , Address (Loc -> Loc)
                     )
newSceneGraph startloc start = do
    (graph, graphAddr) <- foldmp (M.singleton startloc start) return
    (loc, locAddr)     <- foldmp startloc return
    return ( currentScene graph loc
           , addScene graphAddr
           , locAddr
           )
  where
      addScene addr = addr . mappend . uncurry M.singleton
      currentScene = (join .) . liftM2 (M.!)

