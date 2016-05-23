{-# LANGUAGE ScopedTypeVariables #-}

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
                     , Loc -> PropId -> B (Maybe Prop)
                     )
newSceneGraph = do
    let startloc = Loc $ -1
    (graph, addScene) <- newCollection . M.singleton startloc $ pure []
    (loc, setLoc)     <- scanle const startloc
    return ( join . fmap fromJust $ loc >>= graph
           , addScene
           , setLoc
           , getScene graph
           )
  where
    getScene graph loc prop = do
        sceneMay <- graph loc
        case ( sceneMay >>= \scene ->
            return $
                scene >>= return . filter ( maybe False (== prop)
                                          . view propKey
                                          . getTag
                                          )
         ) of
            Just y  -> fmap (fst <$>) (uncons <$> y)
            Nothing -> return Nothing

newTimedCollection :: B Time
                   -> Now ( B [a]
                          , a -> Time -> IO ()
                          )
newTimedCollection clock = do
    (col, add) <- foldmp [] $ \col -> do
        dt <- sample clock
        return . filter ((<= 0) . snd)
               $ map (second $ subtract dt) col
    return ( fmap fst <$> col
           , \a dur -> add ((a, dur) :)
           )

