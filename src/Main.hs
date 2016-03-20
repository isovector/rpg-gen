{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Debug.Trace
import Game.Gen
import Game.Gen.City
import Game.Gen.Player
import Game.Scene
import Game.Sequoia
import Game.Sequoia.Combinators
import Game.Sequoia.Geometry (tryMove)
import Game.Sequoia.Keyboard

player :: Signal Player
player = picking playerGen $ \myPlayer ->
    foldp update myPlayer $ (,,,,)
        <$> collisionMap
        <*> interactions
        <*> elapsed
        <*> arrows
        <*> isDown SpaceKey
  where
    update (walls, ints, dt, dir, active) player@(Player p s) =
        if active
           then trace (concat ints) player
           else let dpos = flip scaleRel dir $ dt * s
                 in flip Player s $ tryMove walls p dpos

city :: Signal [Prop]
city = picking cityGen $ pure . surroundings

gameScene :: Signal [Prop]
gameScene = (liftM2 (.) focusing (:)) <$> fmap prop player
                                      <*> city

interactions :: Signal [String]
interactions =
    delay [] 1 $
        ( \ps p ->
          map (maybe undefined (\(Interactive s) -> s) . getTag)
        . filter (maybe False isInteractive . getTag)
        $ overlapping ps p
        ) <$> city <*> fmap prop player

collisionMap :: Signal [Prop]
collisionMap = delay [] 1 $
    filter (maybe False (== Wall) . getTag) <$> city

main :: IO ()
main = run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

