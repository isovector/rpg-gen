{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Debug.Trace
import Control.Monad.Writer
import Control.Arrow ((***))
import FRP.Helm
import FRP.Helm.Color (Color (), rgb)
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import System.IO.Unsafe (unsafePerformIO)

import Data.Some
import Game.World
import Game.WorldGen

data Player = Player
    { pos :: (Double, Double)
    , speed :: Double
    , color :: Color
    }

data City = City
    { roads :: [Road]
    , buildings :: ()
    }

data Road = Road (Double, Double) (Double, Double) Color

cityGen :: Some City
cityGen = City <$> roadsGen <*> pure ()

roadsGen :: Some [Road]
roadsGen = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    downCount :: Int <- uniformIn (2, 5)
    rightCount :: Int <- uniformIn (2, 5)

    return . snd . runWriter $ do
        let perRight = width / (fromIntegral rightCount - 1)
            perDown = height / (fromIntegral downCount - 1)
            xShift = width / 2
            yShift = height / 2
        tell $ do
            i <- [0 .. downCount - 1]
            return $ Road (0, -yShift + fromIntegral i * perDown) (width, 10) grey
        tell $ do
            i <- [0 .. rightCount - 1]
            return $ Road (-xShift + fromIntegral i * perRight, 0) (10, height) grey


playerGen :: Some Player
playerGen = Player
    <$> pure (0, 0)
    <*> pure 300
    <*> (rgb
        <$> uniformIn (0, 1)
        <*> uniformIn (0, 1)
        <*> uniformIn (0, 1)
        )

normalize :: (Floating a, Eq a, Show a) => (a, a) -> (a, a)
normalize v@(x, y) =
    let lensqr = x^2 + y^2
        len = sqrt lensqr
     in if lensqr /= 0
           then (x / len, y / len)
           else v

player :: Signal Player
player = unsafePerformIO $ do
    myPlayer <- some playerGen
    return . foldp update myPlayer $ (,) <$> Time.elapsed <*> Keyboard.arrows
  where
    update (dt, dpos) p =
        let (x, y)   = pos p
            (dx, dy) = normalize $ join (***) fromIntegral dpos
            s        = speed p
         in p { pos = ( x + dx * s * dt
                      , y + dy * s * dt
                      )
              }

drawCity :: City -> Form
drawCity City{..} = group $ map drawRoad roads
  where
    drawRoad (Road pos size col) = move pos . filled col $ uncurry rect size


drawPlayer :: Player -> Form
drawPlayer p = filled (color p) $ rect 40 40

draw :: Player -> City -> (Int, Int) -> Element
draw p city dims = uncurry centeredCollage dims
    [ move (join (***) negate $ pos p) $ drawCity city
    , drawPlayer p
    ]

city :: Signal City
city = pure . unsafePerformIO $ some cityGen

main :: IO ()
main = run config $ draw <$> player <*> city <*> Window.dimensions
  where
    config = defaultConfig { windowTitle = "rpg-gen" }
