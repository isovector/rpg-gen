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

cityGen :: Some City
cityGen = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    interaction <- interactiveGen
    (\x y -> City  (y : x)) <$> surroundingsGen width height
                            <*> pure interaction

interactiveGen :: Some Prop
interactiveGen = do
    x <- uniformIn (-100, 100)
    y <- uniformIn (-100, 100)
    return . tag (Interactive 1)
           . filled red
           $ rect (mkPos x y) 40 40

surroundingsGen :: Double -> Double -> Some [Prop]
surroundingsGen width' height' = do
    let width = width' * 2
        height = height' * 2
    numTrees <- uniformIn (50, 150)
    trees <- listOf numTrees treeGen

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

treeGen :: Some Prop
treeGen = do
    color <- rgb <$> uniformIn (0.2, 0.4)
                 <*> uniformIn (0.5, 1.0)
                 <*> uniformIn (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . tag Wall
           . filled color
           $ polygon origin [ mkRel 0 (-height / 2)
                            , mkRel  (width / 2) (height / 2)
                            , mkRel (-width / 2) (height / 2)
                            ]

