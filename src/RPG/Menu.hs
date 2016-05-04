{-# LANGUAGE TemplateHaskell #-}
module RPG.Menu
    (
    ) where

import Data.Maybe (isJust)
import Data.Map (Map)
import Game.Sequoia.Keyboard
import RPG.Core
import qualified Data.Map as M

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
           -> Now ( B (Maybe MenuState)
                  , MenuId -> [MenuItem] -> IO ()
                  , Maybe MenuId -> IO ()
                  )
newMenuSet keys = do
    (getMenu, addMenu)  <- newCollection M.empty
    (curMenu, setMenu)  <- scanle const Nothing
    (select,  doSelect) <- scanle (+) 0

    -- TODO(sandy): Find a way to abstract this over UpKey as well.
    onEvent (keyPress keys DownKey) . const $ do
        menuid <- sample curMenu
        when (isJust menuid) . sync $ doSelect 1

    return
        ( do
            selected <- select
            menu     <- curMenu >>= getMenu
            return $ MenuState selected <$> menu
        , addMenu . Just
        , setMenu
        )

