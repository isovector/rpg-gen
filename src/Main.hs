{-# LANGUAGE TemplateHaskell #-}
module Main where

import FRP.Helm
import FRP.Helm.Signal
import FRP.Helm.Sample
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

import Control.Applicative hiding (some)
import Control.Monad
import Data.Some
import Game.World
import Game.WorldGen

gameSignal :: Signal ()
gameSignal = pure ()

world = filled red $ rect 400 700

render :: () -> (Int, Int) -> Element
render _ dims = uncurry centeredCollage dims $ [world]

main :: IO ()
main = run config $ render <$> gameSignal <*> Window.dimensions
  where
    config = defaultConfig { windowTitle = "rpg-gen" }
