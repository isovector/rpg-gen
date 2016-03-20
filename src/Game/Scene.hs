module Game.Scene
    ( Scene
    , changeScene
    , addScene
    , scenes
    ) where

import Preface

import Game.Sequoia
import System.IO.Unsafe (unsafePerformIO)

type Scene = Int

whichScene  :: Signal Scene
changeScene :: Address Scene
(whichScene, changeScene) = unsafePerformIO $ mailbox 0

scenes :: Signal [[Prop]]
addScene :: Address [[Prop]]
(scenes, addScene) = unsafePerformIO $ mailbox []

scene :: Signal [Prop]
scene = (!!) <$> scenes <*> whichScene

