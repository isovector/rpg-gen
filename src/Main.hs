{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Data.List (sortBy)
import Data.Ord (comparing)
import Game.Sequoia.Combinators (focusing)
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Data.Gen.City
import RPG.Menu
import RPG.Player
import RPG.Scene

initialize :: Engine -> N (B [Prop])
initialize engine = mdo
    clock    <- getElapsedClock
    keyboard <- getKeyboard
    (menu, addMenu, setMenu) <- newMenuSet keyboard
    (curScene, addScene, setScene) <- newSceneGraph (Loc 0) city
    city     <- sync . pick $ cityGen addScene setScene (Loc 0)

    (sq, addr) <- run . flip runReader clock
                      . flip runReader keyboard
                      . flip runReader curScene
                      . flip runReader menu
                      $ newPlayer
    return $ do
        p <- sq
        scene <- curScene
        return . focusing p $ p : scene

main = play
    (EngineConfig (640, 480) "rpg-gen")
    initialize $
        return . fmap (sortBy $ comparing $ getY . center)
