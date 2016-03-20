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

player :: Signal Player
player = picking playerGen $ \myPlayer ->
    foldp update myPlayer $ (,,,,)
        <$> collisionMap
        <*> interactions
        <*> elapsed
        <*> arrows
        <*> keyPress SpaceKey
  where
    update (walls, ints, dt, dir, active) player@(Player p s) =
        if active && (not $ null ints)
           then mailing changeScene (head ints) . flip Player s $
                    teleport origin p
           else let dpos = flip scaleRel dir $ dt * s
                 in flip Player s $ tryMove walls p dpos

gameScene :: Signal [Prop]
gameScene = (liftM2 (.) focusing (:)) <$> fmap prop player
                                      <*> scene

interactions :: Signal [Int]
interactions =
    delay [] 1 $
        ( \ps p ->
          map (maybe undefined (\(Interactive s) -> s) . getTag)
        . filter (maybe False isInteractive . getTag)
        $ overlapping ps p
        ) <$> scene <*> fmap prop player

collisionMap :: Signal [Prop]
collisionMap = delay [] 1 $
    filter (maybe False (== Wall) . getTag) <$> scene

main :: IO ()
main = do
    city1 <- surroundings <$> pick cityGen
    city2 <- surroundings <$> pick cityGen
    mail' addScene . return $ return city1
    mail' addScene . return $ return city2
    run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

