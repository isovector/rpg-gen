{-# LANGUAGE LambdaCase #-}
module RPG.Logic.Combat
    ( combat
    ) where

import Control.Monad.State hiding (state)
import Data.Default
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Logic.Input
import RPG.Logic.Menu
import RPG.Logic.QuickTime
import RPG.Logic.Combat.Types

data CombatState = CombatState
    { whoseTurn :: Int
    }

instance Default CombatState where
    def = CombatState 0

__MENU   = 0
__MOVE   = 1
__ATTACK = 2

actionMenu :: Menu
actionMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Move"   $ do
                        setState' __MOVE
                        mail inputFilterAddr $ const GameFilter
                   , MenuItem "Attack" $ setState' __ATTACK
                   ]
    }

combat :: Maybe Int -> QuickTime CombatState [Prop]
combat Nothing = do
    put def
    lift $ do
        mail menuAddr $ const actionMenu
        mail inputFilterAddr $ const NoneFilter
    return []
combat (Just s) = do
    cs <- get
    case whoseTurn cs of
      0 -> myTurn
      _ -> undefined
  where
   myTurn :: QuickTime CombatState [Prop]
   myTurn
    | s == __MENU =
        lift $ runMenu
    | s == __MOVE = do
        since <- sinceState
        when (since >= 1.5) $ do
            lift . mail inputFilterAddr $ const NoneFilter
            setState __MENU
        return []
    | s == __ATTACK =
        undefined

