{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Data.Gen.City
import RPG.Player
import RPG.Scene

initialize :: Engine -> N (B [Prop])
initialize engine = mdo
    clock      <- getElapsedClock
    keyboard   <- getKeyboard
    (curScene, addScene, setScene) <- newSceneGraph (Loc 0) city
    city       <- sync . pick $ cityGen addScene (Loc 0)

    (sq, addr) <- run . flip runReader clock
                      . flip runReader keyboard
                      . flip runReader curScene
                      $ newPlayer
    return $ (:) <$> sq <*> city

main = play (EngineConfig (640, 480) "rpg-gen")
            initialize return
