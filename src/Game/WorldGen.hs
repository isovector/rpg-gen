{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Game.WorldGen where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Writer
import Data.Graph (graphFromEdges)
import Data.List (nub)
import Game.World

type WorldGenT m a = WriterT [(Loc, Loc)] m a

runWorldGenT :: Monad m => WorldGenT m a -> m (World, a)
runWorldGenT gen = do
    (a, w) <- runWriterT gen
    let nodes = nub . join . flip map w $ \a -> [fst a, snd a]
        edges = map (join (***) locKey) w
        input = flip map nodes $ \n ->
            ( n
            , locKey n
            , map snd $ filter ((== locKey n) . fst) edges
            )
        (worldGraph, getLocation', getVertex') = graphFromEdges input
    return (World { .. }, a)

link :: Monad m => Loc -> Loc -> WorldGenT m ()
link a b = tell [(a, b), (b, a)]

