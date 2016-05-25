{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Eff
import Control.Eff.Reader.Lazy
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

import Game.Sequoia.Color

with :: Typeable e => e -> Eff (Reader e :> r) w -> Eff r w
with = flip runReader

initialize :: Engine -> N (B Prop)
initialize engine = do
    clock                    <- getElapsedClock
    keyboard                 <- getKeyboard
    (menu, addMenu, setMenu) <- newMenuSet keyboard
    (  curScene
     , addScene
     , setScene'
     , findProp) <- newSceneGraph

    (  tmpScene :: B [B Prop]
     , addTmpObj
     , clearTmp) <- newTimedCollection clock
    let setScene = (clearTmp >>) . setScene'

    (sq, addr) <- run . with clock
                      . with keyboard
                      . with curScene
                      . with menu
                      $ newPlayer

    let loc = Loc 0
    city <- sync . pick . with addScene
                        . with setScene
                        . with findProp
                        . with addr
                        . with addTmpObj
                        $ cityGen loc
    sync $ addScene loc city >> setScene' loc

    sync $ do
        addMenu (MenuId 0)
            [ MenuItem "play game" $ setMenu Nothing
            , MenuItem "goodbye"   $ error "goodbye"
            ]
        -- setMenu . Just $ MenuId 0

    return $ do
        p     <- sq
        scene <- curScene
        tmp   <- join $ fmap sequence tmpScene
        items <- maybeToList <$> menu
        let screen = focusing p $ group [scene, p, group tmp]
        return $ group [screen, Branch items]

main = play
    (EngineConfig (640, 480) "rpg-gen")
    initialize return

