{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Unsafe.Coerce
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
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
        mail playerAddr $ Just . teleport dst . fromJust

{-# NOINLINE player #-}
{-# NOINLINE playerAddr #-}
player :: Signal (Maybe Prop)
playerAddr :: Address (Maybe Prop)
(player, playerAddr) = picking playerGen . flip foldmp $
    \(Just p) -> do
        walls  <- wallMap
        floors <- floorMap
        dt     <- elapsed
        dir    <- arrows' gameInput
        interactionController p
        let dpos = flip scaleRel dir $ dt * 300
        return . Just $ tryMove walls floors p dpos

gameSceneWQuickTimes :: Signal [Prop]
gameSceneWQuickTimes = do
    gs <- gameScene
    qt <- runQuickTime
    return $ gs ++ qt

gameScene :: Signal [Prop]
gameScene = do
    ps <- scene
    p  <- maybe (error "player died") id <$> player
    return . focusing p $ ps ++ [p]

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
    addScene loc city1

    mail' menuAddr $ const mainMenu
    sampleAt 0 $ do
        makeActor playerAddr $ Actor 100 100 0 (unsafeCoerce $ sword 20)

    sampleAt 1 $ do
        start $ combat gameScene (fromJust <$> player)
    run config gameSceneWQuickTimes
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

