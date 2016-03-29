{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Game.Sequoia.Combinators (focusing)
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Logic.QuickTime
import RPG.Data.Gen.City
import RPG.Data.Gen.Player
import RPG.Data.Gen.Portal
import RPG.Logic.Scene
import qualified Data.Map as M

interactionController :: Signal ()
interactionController = do
    ints    <- interactions
    scenes' <- scenes
    active  <- keyPress SpaceKey

    when (active && (not $ null ints)) $ do
        let (loc, i) = head ints
        mail playerAddr $ teleportTo scenes' loc i

player :: Signal Prop
playerAddr :: Address Prop
(player, playerAddr) = picking playerGen . flip foldmp $
    \p -> do
        walls   <- wallMap
        floors  <- floorMap
        dt      <- elapsed
        dir     <- arrows
        interactionController
        let dpos = flip scaleRel dir $ dt * 300
        return $ tryMove walls floors p dpos

gameScene :: Signal [Prop]
gameScene = do
    ps <- scene
    p  <- player
    return . focusing p $ ps ++ [p]

interactions :: Signal [(Loc, Int)]
interactions =
    delay [] 1 $ do
        ps <- scene
        p  <- player
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

main :: IO ()
main = do
    loc <- newLoc
    city1 <- surroundings <$> pick (cityGen loc)
    addScene loc $ return city1

    run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

