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
import qualified Game.Sequoia.Keyboard as Keyboard

player :: Signal Player
player = picking playerGen $ \myPlayer ->
    foldp update myPlayer $ (,,) <$> collisionMap
                                 <*> elapsed
                                 <*> Keyboard.arrows
  where
    update (walls, dt, dir) (Player p s) =
        let dpos = flip scaleRel dir $ dt * s
         in flip Player s $ move dpos p

city :: Signal [Prop]
city = picking cityGen $ \city ->
    eraser (pure $ surroundings city)
           (const True)
           (prop <$> player)

gameScene :: Signal [Prop]
gameScene = (liftM2 (.) focusing (:)) <$> fmap prop player
                                      <*> city

collisionMap :: Signal [Prop]
collisionMap = delay [] 1 $
    filter (maybe False (== Wall) . getTag) <$> gameScene

main :: IO ()
main = run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen"
                          , windowDimensions = (640, 480)
                          }

