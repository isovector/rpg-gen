{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Game.Sequoia.Combinators (focusing)
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Logic.Menu
import RPG.Logic.QuickTime
import RPG.Data.Gen.City
import RPG.Data.Gen.Player
import RPG.Data.Gen.Portal
import RPG.Logic.Scene
import qualified Data.Map as M

interactionController :: Prop -> Signal ()
interactionController p = do
    ints    <- interactions p
    scenes' <- scenes
    active  <- keyPress SpaceKey

    when (active && (not $ null ints)) $ do
        let (l, i) = head ints
            dst    = getEndpoint scenes' l i

        mail changeScene (const l)
        mail playerAddr $ teleport dst

{-# NOINLINE player #-}
{-# NOINLINE playerAddr #-}
player :: Signal Prop
playerAddr :: Address Prop
(player, playerAddr) = picking playerGen . flip foldmp $
    \p -> do
        walls   <- wallMap
        floors  <- floorMap
        dt      <- elapsed
        dir     <- arrows
        interactionController p
        let dpos = flip scaleRel dir $ dt * 300
        return $ tryMove walls floors p dpos

gameScene :: Signal [Prop]
gameScene = do
    ps <- scene
    p  <- player
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
wallMap = delay [] 1 $
    filter (view hasCollision . getTag) <$> scene

floorMap :: Signal [Prop]
floorMap = delay [] 1 $
    filter (view isFloor . getTag) <$> scene

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
    city1 <- surroundings <$> pick (cityGen loc)
    addScene loc $ return city1

    mail' menuAddr $ const mainMenu
    run config $ join gameState
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

