{-# LANGUAGE TemplateHaskell #-}

module RPG.Menu
    ( MenuId (..)
    , MenuItem (..)
    , MenuState
    , newMenuSet
    ) where

import Control.Arrow (first)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Stanza
import RPG.Core
import qualified Data.Map as M
import qualified Data.Text as T

data MenuItem = MenuItem
    { itemLabel  :: String
    , itemAction :: IO ()
    }

data MenuState = MenuState
    { _menuSelected :: Int
    , _menuItems    :: [MenuItem]
    }
$(makeLenses ''MenuState)

data MenuId = MenuId Int deriving (Eq, Ord)

newMenuSet :: B [Key]
           -> Now ( B (Maybe [Prop])
                  , MenuId -> [MenuItem] -> IO ()
                  , Maybe MenuId -> IO ()
                  )
newMenuSet keys = do
    (getMenu, addMenu)  <- newCollection M.empty
    (curMenu, setMenu)  <- scanle const Nothing
    (select,  doSelect) <- scanle (+) 0

    onEvent (keyPress keys SpaceKey) . const $ do
        menu     <- sample $ curMenu >>= getMenu
        selected <- sample select
        when (isJust menu) . sync
                           . itemAction
                           $ fromJust menu !! selected

    onEvent (change $ round . getY <$> arrows keys) $ \y -> do
        menuid <- sample curMenu
        when (isJust menuid) . sync $ doSelect y

    return
        ( do
            selected <- select
            menu     <- curMenu >>= getMenu
            return $ drawMenu . MenuState selected <$> menu
        , addMenu . Just
        , setMenu
        )

drawMenu :: MenuState -> [Prop]
drawMenu m = map (\(i, p) -> move (scaleRel i (mkRel 0 16)) p)
           . zip [0..]
           . map (uncurry drawMenuItem)
           . map (first (_menuSelected m ==))
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

