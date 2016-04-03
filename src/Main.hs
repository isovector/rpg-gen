{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Unsafe.Coerce
import Control.Monad.IO.Class (liftIO)
import Game.Sequoia.Color
import Game.Sequoia.Combinators (focusing)
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Logic.Combat
import RPG.Logic.Input
import RPG.Logic.Menu
import RPG.Logic.QuickTime
import RPG.Logic.Combat.Types
import RPG.Data.Gen.City
import RPG.Data.Gen.Player
import RPG.Data.Gen.Portal
import RPG.Logic.Scene
import qualified Data.Map as M

interactionController :: Prop -> Signal ()
interactionController p = do
    ints    <- interactions p
    scenes' <- scenes
    active  <- keyPress' gameInput SpaceKey

    when (active && (not $ null ints)) $ do
        let (l, i) = head ints
            dst    = getEndpoint scenes' l i

        mail changeScene (const l)
        mail playerAddr $ teleport dst

badGuy1 :: Signal Prop
(badGuy1, badGuyAddr1) =
    foldmp (filled red $ rect (mkPos (-40) 0) 10 20) return

badGuy2 :: Signal Prop
(badGuy2, badGuyAddr2) =
    foldmp (filled blue $ rect (mkPos 40 0) 20 10) return

{-# NOINLINE player #-}
{-# NOINLINE playerAddr #-}
player :: Signal Prop
playerAddr :: Address Prop
(player, playerAddr) = picking playerGen . flip foldmp $
    \p -> do
        walls  <- wallMap
        floors <- floorMap
        dt     <- elapsed
        dir    <- arrows' gameInput
        interactionController p
        let dpos = flip scaleRel dir $ dt * 300
        return $ tryMove walls floors p dpos

gameSceneWQuickTimes :: Signal [Prop]
gameSceneWQuickTimes = do
    gs <- gameScene
    qt <- runQuickTime
    return $ gs ++ qt

gameScene :: Signal [Prop]
gameScene = do
    ps <- scene
    bg1 <- badGuy1
    bg2 <- badGuy2
    p  <- player
    return . focusing p $ ps ++ [bg1, bg2, p]

interactions :: Prop -> Signal [(Loc, Int)]
interactions p = do
    ps <- scene
    return . map (maybe undefined $ \(Teleport s i) -> (s, i))
           . map (view interaction)
           . filter hasInteraction
           . map getTag
           $ overlapping ps p

wallMap :: Signal [Prop]
wallMap = filter (view hasCollision . getTag) <$> scene

floorMap :: Signal [Prop]
floorMap = filter (view isFloor . getTag) <$> scene

mainMenu :: Menu
mainMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Start Game" . mail gameStateAddr
                                           $ const gameScene
                   , MenuItem "Quit" $ error "goodbye"
                   ]
    }

main :: IO ()
main = do
    loc <- newLoc
    city1 <- pick (cityGen loc)
    addScene loc $ return city1

    mail' menuAddr $ const mainMenu
    sampleAt 0 $ do
        makeActor badGuyAddr1 $ Actor 100 100 1 (unsafeCoerce $ sword 20)
        makeActor badGuyAddr2 $ Actor 100 100 1 (unsafeCoerce $ sword 20)
        makeActor playerAddr $ Actor 100 100 0 (unsafeCoerce $ sword 20)

    sampleAt 1 $ do
        start $ combat gameScene player
    run config gameSceneWQuickTimes
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

