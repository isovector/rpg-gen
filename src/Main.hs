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

player :: Signal Player
player = picking playerGen $ \myPlayer ->
    foldp update myPlayer $ (,,,,,,)
        <$> wallMap
        <*> floorMap
        <*> interactions
        <*> elapsed
        <*> arrows
        <*> keyPress SpaceKey
        <*> scenes
  where
    update (walls, floors, ints, dt, dir, active, scenes)
           player@(Player p s) =
        if active && (not $ null ints)
           then let (loc, i) = head ints
                 in flip Player s $ teleportTo scenes loc i p
           else
                let dpos = flip scaleRel dir $ dt * s
                 in flip Player s $ tryMove walls floors p dpos

playerProp :: Signal Prop
playerProp = fmap prop player

gameScene :: Signal [Prop]
gameScene = do
    ps <- scene
    p  <- playerProp
    return . focusing p $ ps ++ [p]

interactions :: Signal [(Loc, Int)]
interactions =
    delay [] 1 $ do
        ps <- scene
        p  <- playerProp
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

