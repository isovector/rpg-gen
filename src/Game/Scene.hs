module Game.Scene
    ( Scene
    , changeScene
    , addScene
    , scene
    ) where

import Preface

import Game.Sequoia
import System.IO.Unsafe (unsafePerformIO)

type Scene = Int

whichScene  :: Signal  Scene
changeScene :: Address Scene
(whichScene, changeScene) = unsafePerformIO $ mailbox 0

scenes   :: Signal  [Signal [Prop]]
addScene :: Address [Signal [Prop]]
(scenes, addScene) = unsafePerformIO $ mailboxs (++) []

scene :: Signal [Prop]
scene = join $ (!!) <$> scenes <*> whichScene

