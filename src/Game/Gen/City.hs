module Game.Gen.City
    ( City (..)
    , cityGen
    ) where

import Preface

import Data.Some
import Game.Sequoia
import Game.Sequoia.Color (rgb, red)

data City = City
    { surroundings :: [Prop]
    }

cityGen :: Prop -> Some City
cityGen p = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    surroundings <- surroundingsGen width height
    pdx <- (/ 1.5) <$> uniformIn (-width, width)
    pdy <- (/ 1.5) <$> uniformIn (-height, height)
    return . City $ move (mkRel pdx pdy) p : surroundings

surroundingsGen :: Double -> Double -> Some [Prop]
surroundingsGen width' height' = do
    let width = width' * 2
        height = height' * 2
    obstacleGen <- uniformly [treeGen, rockGen]
    numTrees <- uniformIn (50, 150)
    trees <- listOf numTrees obstacleGen

    forM trees $ \tree -> do
        (xoffset', yoffset') <- uniformly [ (-1,  0)
                                          , ( 1,  0)
                                          , ( 0, -1)
                                          , ( 0,  1)
                                          ]
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

rockGen :: Some Prop
rockGen = do
    color <- rgb <$> uniformIn (0.3, 0.8)
                 <*> uniformIn (0, 0.3)
                 <*> uniformIn (0.3, 1)
    radius <- uniformIn (10, 30)
    return . tags (hasCollision .~ True)
           . filled color
           $ circle origin radius

treeGen :: Some Prop
treeGen = do
    color <- rgb <$> uniformIn (0.2, 0.4)
                 <*> uniformIn (0.5, 1.0)
                 <*> uniformIn (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . tags (hasCollision .~ True)
           . filled color
           $ polygon origin [ mkRel 0 (-height / 2)
                            , mkRel  (width / 2) (height / 2)
                            , mkRel (-width / 2) (height / 2)
                            ]

