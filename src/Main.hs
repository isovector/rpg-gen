{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Game.Gen
import Game.Gen.City
import Game.Gen.Player
import Game.Scene
import Game.Sequoia
import Game.Sequoia.Combinators
import Game.Sequoia.Geometry (tryMove)
import Game.Sequoia.Keyboard
import qualified Data.Map as M

player :: Signal Player
player = picking playerGen $ \myPlayer ->
    foldp update myPlayer $ (,,,,,)
        <$> collisionMap
        <*> interactions
        <*> elapsed
        <*> arrows
        <*> keyPress SpaceKey
        <*> scenes
  where
    update (walls, ints, dt, dir, active, scenes)
           player@(Player p s) =
        if active && (not $ null ints)
           then let (loc, i) = head ints
                 in flip Player s $ teleportTo scenes loc i p
           else
                let dpos = flip scaleRel dir $ dt * s
                 in flip Player s $ tryMove walls p dpos

gameScene :: Signal [Prop]
gameScene = (liftM2 (.) focusing (:)) <$> fmap prop player
                                      <*> scene

interactions :: Signal [(LocKey, Int)]
interactions =
    delay [] 1 $
        ( \ps p ->
          map ( maybe undefined (\(Teleport s i) -> (s, i))
              . view interaction
              . getTag
              )
        . filter (hasInteraction . getTag)
        $ overlapping ps p
        ) <$> scene <*> fmap prop player

collisionMap :: Signal [Prop]
collisionMap = delay [] 1 $
    filter (view hasCollision . getTag) <$> scene

main :: IO ()
main = do
    (p1, p2) <- pick $ portal 0 1
    city1 <- surroundings <$> pick (cityGen p1)
    city2 <- surroundings <$> pick (cityGen p2)
    mail' addScene . M.singleton 0 $ return city1
    mail' addScene . M.singleton 1 $ return city2
    run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

