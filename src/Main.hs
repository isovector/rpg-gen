module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Game.Sequoia.Keyboard
import RPG.Core
-- import RPG.Data.Gen.City
import RPG.Player
import RPG.Scene

initialize :: Engine -> N (B Prop)
initialize engine = do
    clock      <- getElapsedClock
    keyboard   <- getKeyboard
    -- city       <- sync $ pick cityGen
    (curScene, addScene, setScene) <- newSceneGraph (Loc 0) $ pure []

    (sq, addr) <- run . flip runReader clock
                      . flip runReader keyboard
                      . flip runReader curScene
                      $ newPlayer
    return sq

main = play (EngineConfig (640, 480) "rpg-gen")
            initialize $ \sq -> do
                return $ fmap return sq
