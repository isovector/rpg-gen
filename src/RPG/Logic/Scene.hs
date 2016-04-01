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
import RPG.Core
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Data.Traversable as T

{-# NOINLINE locIdGen #-}
locIdGen = unsafePerformIO $ newIORef (Loc 0)

newLoc :: IO Loc
newLoc = do
    loc@(Loc i) <- readIORef locIdGen
    writeIORef locIdGen . Loc $ i + 1
    return loc

{-# NOINLINE currentLoc #-}
{-# NOINLINE changeScene #-}
currentLoc  :: Signal Loc
(currentLoc, changeScene) = newMailbox "current scene" 0

{-# NOINLINE allScenes #-}
{-# NOINLINE allScenesAddr #-}
allScenes :: Signal (Map Loc (Signal [Prop]))
(allScenes, allScenesAddr) = newMailbox "all scenes" M.empty

addScene :: Loc -> Signal [Prop] -> IO ()
addScene loc s = mail' allScenesAddr . mappend $ M.singleton loc s

scene :: Signal [Prop]
scene = join $ (M.!) <$> allScenes <*> currentLoc

scenes :: Signal (Map Loc [Prop])
scenes = effectful $ \i ->
    T.mapM (sampleAt i) =<< runSignal allScenes i

getEndpoint :: Map Loc [Prop] -> Loc -> Int -> Pos
getEndpoint ls l i = maybe (error $ "unknown " ++ show l) id $ do
    loc <- M.lookup l ls
    dst <- findProp loc i
    return $ center dst

findProp :: [Prop] -> Int -> Maybe Prop
findProp ps i = find (maybe False (== i) . view propKey . getTag) ps

