module RPG.Logic.Scene
    ( changeScene
    , currentLoc
    , addScene
    , scene
    , scenes
    , teleportTo
    , findProp
    ) where

import Data.List (find)
import RPG.Core
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Data.Traversable as T

{-# NOINLINE currentLoc #-}
{-# NOINLINE changeScene #-}
currentLoc  :: Signal  Loc
changeScene :: Address Loc
(currentLoc, changeScene) = newMailbox "current scene" 0

{-# NOINLINE allScenes #-}
{-# NOINLINE addScene #-}
allScenes :: Signal  (Map Loc (Signal [Prop]))
addScene  :: Address (Map Loc (Signal [Prop]))
(allScenes, addScene) = newMailbox "all scenes" M.empty

scene :: Signal [Prop]
scene = join $ (M.!) <$> allScenes <*> currentLoc

scenes :: Signal (Map Loc [Prop])
scenes = effectful $ \i ->
    T.mapM (sampleAt i) =<< runSignal allScenes i

teleportTo :: Map Loc [Prop] -> Loc -> Int -> Prop -> Prop
teleportTo ls l i p = maybe p id $ do
    loc <- M.lookup l ls
    dst <- findProp loc i
    return . mailing changeScene (const l)
           $ teleport (center dst) p

findProp :: [Prop] -> Int -> Maybe Prop
findProp ps i = find (maybe False (== i) . view propKey . getTag) ps

