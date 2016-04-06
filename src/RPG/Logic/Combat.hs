{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Combat
    ( startCombat
    ) where

import Control.Arrow (first, second)
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

data CombatState = CSMenu
                 | MovePrep
                 | Move
                 | AttackInit
                 | AttackSelect
                 deriving (Eq, Show, Ord, Enum)

data CombatData = CombatData
    { _whoseTurn    :: Int
    , _curSelection :: Int
    , _targets      :: [Target]
    , _state        :: CombatState
    }
$(makeLenses ''CombatData)

actionMenu :: Menu
actionMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Move"   $ return () -- setState' MovePrep
                   , MenuItem "Attack" $ return () -- setState' AttackInit
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

setState :: CombatState -> StateT CombatData Signal ()
setState s = modify $ state .~ s

startCombat :: Signal [Prop] -> Signal Prop -> Signal ()
startCombat pss players = do
    mail menuAddr $ const actionMenu
    mail inputFilterAddr $ const NoneFilter
    start $ combat pss players

combat :: Signal [Prop] -> Signal Prop -> Machine [Prop]
combat pss players = machine (CombatData 0 0 [] CSMenu) $ do
    cd     <- get
    ps     <- lift pss
    player <- lift players

    case _whoseTurn cd of
      0 -> myTurn ps player $ _state cd
      _ -> undefined
  where
   _Just' :: Lens' (Maybe a) a
   _Just' = lens fromJust (const Just)

   actor' :: Lens' Prop Actor
   actor' = tagL.propActor._Just'

   toroidSel :: Lens' CombatData Int
   toroidSel = torus targets curSelection

   myTurn :: [Prop] -> Prop -> CombatState -> QuickTime CombatData (Bool, [Prop])
   myTurn ps player s
    | s == CSMenu = lift $ (True,) <$> runMenu

    | s == MovePrep = do
        since <- sinceState
        when (since >= 0.2) $ do
            lift . mail inputFilterAddr $ const GameFilter
            setState Move
        continue []

    | s == Move = do
        since <- sinceState
        when (since >= 1) $ do
            lift . mail inputFilterAddr $ const NoneFilter
            setState CSMenu
        continue []

    | s == AttackInit = do
        let a  = view actor' player
            w  = view weapon a
            ts = sortBy (on compare (fst . unpackPos . location))
                    . filter (isTargetable w a . who)
                    . showTrace
                    . fst
                    $ partitionActors player ps
        modify $ targets .~ ts
        setState AttackSelect
        continue []

    | s == AttackSelect = do
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
            setState CSMenu
            lift . start . action w $ AttackParams
                { src         = a
                , targeted    = [t]
                , environment = ps
                }

        p <- lift . floating
                  . move (mkRel 0 (-10))
                  . teleport pos
                  $ styled red defaultLine arrow
        continue [p]

