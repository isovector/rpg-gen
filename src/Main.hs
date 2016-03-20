{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Debug.Trace

import Control.Arrow ((***))
import Control.Monad.Writer
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Game.Sequoia
import Game.Sequoia.Color (Color (), rgb, grey)
import Game.Sequoia.Geometry (sweepProp, tryMove)
import Game.Sequoia.Utils
import System.IO.Unsafe (unsafePerformIO)
import qualified Game.Sequoia.Keyboard as Keyboard
import qualified Game.Sequoia.Window as Window

import Data.Some
import Game.Combinators
import Game.World
import Game.WorldGen

data Player = Player
    { prop :: Prop
    , speed :: Double
    }

data City = City
    { surroundings :: [Prop]
    }


cityGen :: Some City
cityGen = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    City <$> surroundingsGen width height


surroundingsGen :: Double -> Double -> Some [Prop]
surroundingsGen width' height' = do
    let width = width' * 2
        height = height' * 2
    numTrees <- uniformIn (50, 150)
    trees <- listOf numTrees treeGen

    forM trees $ \tree -> do
        (xoffset', yoffset') <- uniformly [(-1, 0), (1, 0), (0, -1), (0, 1)]
        let xoffset = xoffset' * width'
            yoffset = yoffset' * height'
        xspread <-
            if xoffset' == 0
               then uniformIn (-width / 2, width / 2)
               else uniformIn (-50, 50)
        yspread <-
            if yoffset' == 0
               then (subtract $ height/2) <$> uniformIn (0, height)
               else uniformIn (-50, 50)
        return $ move (mkRel (xoffset + xspread) (yoffset + yspread)) tree

showTag :: Prop -> Prop
showTag = trace =<< show . getTag

treeGen :: Some Prop
treeGen = do
    color <- rgb <$> uniformIn (0.2, 0.4) <*> uniformIn (0.5, 1.0) <*> uniformIn (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . tag Wall
           . filled color
           $ polygon origin [ mkRel 0 (-height / 2)
                            , mkRel  (width / 2) (height / 2)
                            , mkRel (-width / 2) (height / 2)
                            ]

playerGen :: Some Player
playerGen = do
    color <- rgb <$> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
                 <*> uniformIn (0, 1)
    let form = filled color $ rect origin 20 20
    return $ Player form 300

normalize :: (Floating a, Eq a, Show a) => (a, a) -> (a, a)
normalize v@(x, y) =
    let lensqr = x^2 + y^2
        len = sqrt lensqr
     in if lensqr /= 0
           then (x / len, y / len)
           else v

player :: Signal Player
player = unsafePerformIO $ do
    myPlayer <- pick playerGen
    return . foldp update myPlayer $ (,,) <$> collisionMap
                                          <*> elapsed
                                          <*> Keyboard.arrows
  where
    update (walls, dt, dir) (Player p s) =
        let dpos = flip scaleRel dir $ dt * s
         in flip Player s $ move dpos p

draw :: Player -> [Prop] -> [Prop]
draw p city = prop p : city

city :: Signal [Prop]
city = unsafePerformIO $ do
    s <- surroundings <$> pick cityGen
    return $ eraser (pure s) (const True) (prop <$> player)

gameScene :: Signal [Prop]
gameScene = draw <$> player <*> city

collisionMap :: Signal [Prop]
collisionMap = delay [] 1 $ filter (maybe False (== Wall) . getTag) <$>  gameScene

main :: IO ()
main = run config gameScene
  where
    config = EngineConfig { windowTitle = "rpg-gen", windowDimensions = (640, 480) }
