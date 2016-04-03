{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Menu
    ( MenuItem (..)
    , Menu (..)
    , runMenu
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
import RPG.Logic.Utils
import qualified Data.Text as T

data MenuItem = MenuItem
    { itemLabel :: String
    , itemAction :: Signal ()
    -- TODO(sandy): need support for disabled
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

boundedSel :: Lens' Menu Int
boundedSel = bounded menuItems menuSelected

{-# NOINLINE gameState #-}
{-# NOINLINE gameStateAddr #-}
-- TODO(sandy): not sure how I feel about this
gameState :: Signal (Signal [Prop])
(gameState, gameStateAddr) =
    newMailbox "game state" $ runMenu

runMenu :: Signal [Prop]
runMenu = drawMenu <$> menuRealSignal

menuRealSignal :: Signal Menu
menuRealSignal = do
    whenM (keyPress UpKey)   . mail menuAddr $ over boundedSel (subtract 1)
    whenM (keyPress DownKey) . mail menuAddr $ over boundedSel (+1)

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

