{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Menu
    ( MenuItem (..)
    , Menu (..)
    , menuItems
    , menuSelected
    , gameState
    , gameStateAddr
    , menuAddr
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

-- TODO(sandy): maybe eq is too restrictive...
instance Eq Menu where
    m1 == m2 = labels m1 == labels m2
            && _menuSelected m1 == _menuSelected m2
      where labels = map itemLabel . _menuItems

up :: Signal ()
up = do
    isUp <- keyPress UpKey
    when isUp . mail menuAddr $ \s ->
        s & menuSelected .~ (max 0 . subtract 1 $ _menuSelected s)

down :: Signal ()
down = do
    isDown <- keyPress DownKey
    when isDown . mail menuAddr $ \s ->
        s & menuSelected .~
            (min (subtract 1 . length $ _menuItems s)
                $ 1 + _menuSelected s)

{-# NOINLINE gameState #-}
{-# NOINLINE gameStateAddr #-}
gameState :: Signal (Signal [Prop])
(gameState, gameStateAddr) =
    newMailbox "game state" $ drawMenu <$> menuRealSignal

menuRealSignal :: Signal Menu
menuRealSignal = do
    up
    down
    enter <- keyPress SpaceKey
    when enter $ selected >>= itemAction
    menuSignal

{-# NOINLINE menuSignal #-}
{-# NOINLINE menuAddr #-}
menuSignal :: Signal Menu
(menuSignal, menuAddr) = newMailbox "menu" $ error "no menu set"

selected :: Signal MenuItem
selected = do
    menu <- menuSignal
    return $ _menuItems menu !! _menuSelected menu



drawMenu :: Menu -> [Prop]
drawMenu m = map (\(i, p) -> move (scaleRel i (mkRel 0 16)) p)
           . zip [0..]
           . map (uncurry drawMenuItem)
           . map (first $ (_menuSelected m ==))
           . zip [0..]
           $ _menuItems m

drawMenuItem :: Bool -> MenuItem -> Prop
drawMenuItem selected = StanzaProp
                      . monospace
                      . color (if selected then red else blue)
                      . height 12
                      . toStanza
                      . T.pack
                      . itemLabel

