{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Combat
    ( combat
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Data.Default
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils (showTrace)
import RPG.Core
import RPG.Logic.Input
import RPG.Logic.Menu
import RPG.Logic.QuickTime
import RPG.Logic.Combat.Types
import RPG.Logic.Utils

data CombatState = CombatState
    { _whoseTurn    :: Int
    , _curSelection :: Int
    , _targets      :: [Target]
    }
$(makeLenses ''CombatState)

__MENU        = 0
__MOVE        = 1
__ATTACK_INIT = 2
__ATTACK_SEL  = 3

actionMenu :: Menu
actionMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Move"   $ do
                        setState' __MOVE
                        mail inputFilterAddr $ const GameFilter
                   , MenuItem "Attack" $ setState' __ATTACK_INIT
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

combat :: Signal [Prop]
       -> Signal Prop
       -> Maybe Int
       -> QuickTime CombatState [Prop]
combat pss players Nothing = do
    put $ CombatState 0 0 []
    lift $ do
        mail menuAddr $ const actionMenu
        mail inputFilterAddr $ const NoneFilter
    return []
combat pss players (Just s) = do
    cs     <- get
    ps     <- lift pss
    player <- lift players

    case _whoseTurn cs of
      0 -> myTurn ps player
      _ -> undefined
  where
   _Just' :: Lens' (Maybe a) a
   _Just' = lens fromJust (const Just)

   actor' :: Lens' Prop Actor
   actor' = tagL.propActor._Just'

   toroidSel :: Lens' CombatState Int
   toroidSel = torus targets curSelection

   myTurn :: [Prop] -> Prop -> QuickTime CombatState [Prop]
   myTurn ps player
    | s == __MENU = lift $ runMenu

    | s == __MOVE = do
        since <- sinceState
        when (since >= 1.5) $ do
            lift . mail inputFilterAddr $ const NoneFilter
            setState __MENU
        return []

    | s == __ATTACK_INIT = do
        let a  = view actor' player
            w  = view weapon a
            ts = sortBy (on compare (fst . unpackPos . location))
                    . filter (isTargetable w a . who)
                    . showTrace
                    . fst
                    $ partitionActors player ps
        modify $ targets .~ ts
        setState __ATTACK_SEL
        return []

    | s == __ATTACK_SEL = do
        st <- get
        let ts   = view targets st
            sel  = view toroidSel st
            t    = ts !! sel
            pos = location t
        selected <- lift $ keyPress SpaceKey

        whenM (lift $ keyPress RightKey) . modify $ over toroidSel (+1)
        whenM (lift $ keyPress LeftKey)  . modify $ over toroidSel (subtract 1)

        when selected $ do
            let a  = view actor' player
                w  = view weapon a
            setState __MENU
            lift . start . action w $ AttackParams
                { src         = a
                , targeted    = [t]
                , environment = ps
                }

        p <- lift . floating
                  . move (mkRel 0 (-10))
                  . teleport pos
                  $ styled red defaultLine arrow
        return [p]

