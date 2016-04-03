{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Combat
    ( combat
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Data.Default
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import RPG.Core
import RPG.Logic.Input
import RPG.Logic.Menu
import RPG.Logic.QuickTime
import RPG.Logic.Combat.Types

data CombatState = CombatState
    { _whoseTurn    :: Int
    , _curSelection :: Int
    , _targets      :: [Target]
    }
$(makeLenses ''CombatState)

__MENU       = 0
__MOVE       = 1
__ATTACK_SEL = 2

actionMenu :: Menu
actionMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Move"   $ do
                        setState' __MOVE
                        mail inputFilterAddr $ const GameFilter
                   , MenuItem "Attack" $ setState' __ATTACK_SEL
                   ]
    }

arrow :: Shape
arrow = polygon origin [ mkRel 0     0
                       , mkRel (-10) (-20)
                       , mkRel (-5)  (-20)
                       , mkRel (-5)  (-50)
                       , mkRel 5     (-50)
                       , mkRel 5     (-20)
                       , mkRel 10    (-20)
                       ]

floating :: Prop -> Signal Prop
floating p = do
    t <- time
    return $ move (mkRel 0 . (* 10) . cos $ 4 * t) p

combat :: Maybe Int -> QuickTime CombatState [Prop]
combat Nothing = do
    put $ CombatState 0 0 []
    lift $ do
        mail menuAddr $ const actionMenu
        mail inputFilterAddr $ const NoneFilter
    return []
combat (Just s) = do
    cs <- get
    case _whoseTurn cs of
      0 -> myTurn
      _ -> undefined
  where
   myTurn :: QuickTime CombatState [Prop]
   myTurn
    | s == __MENU = lift $ runMenu
    | s == __MOVE = do
        since <- sinceState
        when (since >= 1.5) $ do
            lift . mail inputFilterAddr $ const NoneFilter
            setState __MENU
        return []
    | s == __ATTACK_SEL = do
        p <- lift . floating
                  . move (mkRel 0 (-10))
                  $ styled red defaultLine arrow
        return [p]

