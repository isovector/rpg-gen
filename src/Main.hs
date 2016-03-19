{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Debug.Trace
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Control.Monad.Writer
import Control.Arrow ((***))
import FRP.Helm
import FRP.Helm.Color (Color (), rgb)
import FRP.Helm.Geometry (BB, blocks, tagged, sweepShape)
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
    , surroundings :: [Prop]
    }

data Road = Road (Double, Double) (Double, Double) Color

data Block = Track | Wall deriving Eq

type Prop = Prop' Block
type Scene = Scene' Block


cityGen :: Some City
cityGen = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    City <$> roadsGen width height <*> surroundingsGen width height


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
        return $ move (xoffset + xspread, yoffset + yspread) tree

treeGen :: Some Prop
treeGen = do
    color <- rgb <$> uniformIn (0.2, 0.4) <*> uniformIn (0.5, 1.0) <*> uniformIn (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . group $
        [ filled color $ polygon [(0, -height), (width / 2, 0), (-width / 2, 0)]
        , move (0, -height / 2) $ block Wall width height
        ]

roadsGen :: Double -> Double -> Some [Road]
roadsGen width height = do
    downCount :: Int <- uniformIn (2, 5)
    rightCount :: Int <- uniformIn (2, 5)
    downRandom <- listOf downCount $ uniformIn (-15, 15)
    rightRandom <- listOf rightCount $ uniformIn (-15, 15)

    let perRight = width / (fromIntegral rightCount + 1)
        perDown = height / (fromIntegral downCount + 1)
        xShift = width / 2
        yShift = height / 2

    return . snd . runWriter $ do
        let getPos shift i per rand = -shift + fromIntegral i * per + rand !! (i - 1)
        tell $ do
            i <- [1 .. downCount]
            return $ Road (0, getPos yShift i perDown downRandom) (width, 10) grey
        tell $ do
            i <- [1 .. rightCount]
            return $ Road (getPos xShift i perRight rightRandom, 0) (10, height) grey


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
    return . foldp update myPlayer $ (,,) <$> collisionMap
                                          <*> Time.elapsed
                                          <*> Keyboard.arrows
  where
    update (bbs, dt, dpos) p =
        let here     = safeHead $ tagged bbs Track
            walls    = tagged bbs Wall
            s        = speed p
            rel@(dx, dy) = let (dx', dy') = normalize $
                                    join (***) fromIntegral dpos
                            in (dx' * s * dt, dy' * s * dt)
            collides = isJust $ sweepShape walls here rel
            (x, y)   = pos p
         in if collides
               then p
               else p { pos = ( x + dx
                              , y + dy
                              )}

drawCity :: City -> Prop
drawCity City{..} = group $ map drawRoad roads ++ surroundings
  where
    drawRoad (Road pos size col) = move pos . filled col $ uncurry rect size


drawPlayer :: Player -> Prop
drawPlayer p = group [ filled (color p) $ rect 40 40
                     , block Track 40 40
                     ]

draw :: Player -> City -> (Int, Int) -> Scene
draw p city dims = uncurry centeredCollage dims
    [ move (join (***) negate $ pos p) $ drawCity city
    , drawPlayer p
    ]

city :: Signal City
city = pure . unsafePerformIO $ some cityGen

gameScene :: Signal Scene
gameScene = draw <$> player <*> city <*> Window.dimensions

collisionMap :: Signal [BB Block]
collisionMap = delay [] 1 $ blocks <$> gameScene

main :: IO ()
main = run config gameScene
  where
    config = defaultConfig { windowTitle = "rpg-gen" }
