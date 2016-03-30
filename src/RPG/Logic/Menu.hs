{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Menu
    ( MenuItem (..)
    , Menu ()
    , menuItems
    , menuSelected
    , testMenu
    , drawMenu
    ) where

import Control.Arrow (first)
import Control.Lens
import Control.Lens.TH
import Control.Monad.IO.Class (liftIO)
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Stanza
import RPG.Core
import qualified Data.Text as T

data MenuItem = MenuItem
    { itemLabel :: String
    , itemAction :: Signal ()
    }

data Menu = Menu
    { _menuItems    :: [MenuItem]
    , _menuSelected :: Int
    }
$(makeLenses ''Menu)

testMenu :: Menu
testMenu = Menu
    { _menuSelected = 1
    , _menuItems = [ MenuItem "hello" $ return ()
                   , MenuItem "goodbye" $ return ()
                   , MenuItem "nice it works" $ return ()
                   ]
    }


drawMenu :: Menu -> [Prop]
drawMenu m = map (\(i, p) -> move (scaleRel i (mkRel 0 14)) p)
           . zip [0..]
           . map (uncurry drawMenuItem)
           . map (first $ (_menuSelected m ==))
           . zip [0..]
           $ _menuItems m

drawMenuItem :: Bool -> MenuItem -> Prop
drawMenuItem selected = StanzaProp
                      . monospace
                      . color (if selected then red else blue)
                      . toStanza
                      . T.pack
                      . itemLabel

