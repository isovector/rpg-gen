module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Player

initialize :: Engine -> N (B Prop)
initialize engine = do
    clock      <- getElapsedClock
    keyboard   <- getKeyboard
    (sq, addr) <- run . flip runReader clock
                      . flip runReader keyboard
                      . flip runReader (pure [] :: B [Prop])
                      $ newPlayer
    return sq

main = play (EngineConfig (640, 480) "rpg-gen")
            initialize $ \sq -> do
                return $ fmap return sq
