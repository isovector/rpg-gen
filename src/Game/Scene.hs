module Game.Scene
    ( changeScene
    , currentLoc
    , addScene
    , scene
    , scenes
    ) where

import Preface

import Game.Sequoia
import System.IO.Unsafe (unsafePerformIO)
import Data.Map (Map (..))
import qualified Data.Map as M
import qualified Data.Traversable as T

currentLoc  :: Signal  LocKey
changeScene :: Address LocKey
(currentLoc, changeScene) = unsafePerformIO $ mailbox 0

allScenes :: Signal  (Map LocKey (Signal [Prop]))
addScene  :: Address (Map LocKey (Signal [Prop]))
(allScenes, addScene) = unsafePerformIO $ mailboxs mappend M.empty

scene :: Signal [Prop]
scene = join $ (M.!) <$> allScenes <*> currentLoc

scenes :: Signal (Map LocKey [Prop])
scenes = effectful $ \i ->
    T.mapM (sampleAt i) =<< runSignal allScenes i

