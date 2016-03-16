{-# LANGUAGE TemplateHaskell #-}
module Main where

import Preface

import Debug.Trace
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

renderPlayer :: Player -> Form
renderPlayer p = move (pos p) . filled (color p) $ rect 40 40

render :: Player -> (Int, Int) -> Element
render p dims = uncurry centeredCollage dims . return $ renderPlayer p

main :: IO ()
main = run config $ render <$> player <*> Window.dimensions
  where
    config = defaultConfig { windowTitle = "rpg-gen" }
