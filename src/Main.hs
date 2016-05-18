{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
import Data.IORef
import Data.List (sortBy)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Typeable (Typeable)
import Game.Sequoia.Combinators (focusing)
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Data.Gen.City
import RPG.Menu
import RPG.Player
import RPG.Scene
import qualified Data.Map as M

with :: Typeable e => e -> Eff (Reader e :> r) w -> Eff r w
with = flip runReader

initialize :: Engine -> N (B [Prop])
initialize engine = mdo
    clock    <- getElapsedClock
    keyboard <- getKeyboard
    (  findProp
     , registerProp :: Int -> Prop -> IO ()) <- newCollection M.empty
    (menu, addMenu, setMenu) <- newMenuSet keyboard
    (curScene, addScene, setScene) <- newSceneGraph (Loc 0) city

    (ioRef :: IORef ((Prop -> Prop) -> IO ())) <- sync $ newIORef undefined

    city <- sync . pick . with addScene
                        . with setScene
                        . with findProp
                        . with registerProp
                        . with ioRef
                        $ cityGen (Loc 0)


    (sq, addr) <- run . with clock
                      . with keyboard
                      . with curScene
                      . with menu
                      $ newPlayer
    sync $ writeIORef ioRef addr

    sync $ do
        addMenu (MenuId 0)
            [ MenuItem "play game" $ setMenu Nothing
            , MenuItem "goodbye"   $ error "goodbye"
            ]
        setMenu . Just $ MenuId 0

    return $ do
        p <- sq
        scene <- curScene
        let screen = focusing p $ p : scene
        items <- join . maybeToList <$> menu
        return $ screen ++ items

main = play
    (EngineConfig (640, 480) "rpg-gen")
    initialize return
