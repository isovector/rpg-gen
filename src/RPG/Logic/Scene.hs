module RPG.Logic.Scene
    ( changeScene
    , currentLoc
    , addScene
    , scene
    , scenes
    , getEndpoint
    , findProp
    , newLoc
    ) where

import Data.IORef
import Data.List (find)
import Data.Maybe (catMaybes)
import RPG.Core
import RPG.Logic.Utils
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Data.Traversable as T

{-# NOINLINE locIdGen #-}
locIdGen = unsafePerformIO $ newIORef (Loc 0)

newLoc :: IO Loc
newLoc = newX (\(Loc i) -> Loc $ i + 1) locIdGen

{-# NOINLINE currentLoc #-}
{-# NOINLINE changeScene #-}
currentLoc  :: Signal Loc
(currentLoc, changeScene) = newMailbox "current scene" $ Loc 0

{-# NOINLINE allScenes #-}
{-# NOINLINE allScenesAddr #-}
allScenes :: Signal (Map Loc (Signal [Maybe Prop]))
(allScenes, allScenesAddr) = newMailbox "all scenes" M.empty

{-# NOINLINE addScene #-}
addScene :: Loc -> Signal [Maybe Prop] -> IO ()
addScene = addX' allScenesAddr

scene :: Signal [Prop]
scene = fmap catMaybes . join $ getCurX allScenes currentLoc

scenes :: Signal (Map Loc [Prop])
scenes = effectful $ \i ->
    T.mapM (fmap catMaybes . sampleAt i) =<< runSignal allScenes i

getEndpoint :: Map Loc [Prop] -> Loc -> Int -> Pos
getEndpoint ls l i = maybe (error $ "unknown " ++ show l) id $ do
    loc <- M.lookup l ls
    dst <- findProp loc i
    return $ center dst

findProp :: [Prop] -> Int -> Maybe Prop
findProp ps i = find (maybe False (== i) . view propKey . getTag) ps

