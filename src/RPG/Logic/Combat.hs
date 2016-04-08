{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Combat
    ( startCombat
    , makeActor
    , sword
    ) where

import Control.Arrow (first, second)
import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Data.Default
import Data.Function (on)
import Data.List (partition, sortBy)
import Data.Maybe (fromJust)
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Utils (showTrace)
import RPG.Core
import RPG.Logic.Input
import RPG.Logic.Menu
import RPG.Logic.QuickTime
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
    , _stateTime    :: Time
    }
$(makeLenses ''CombatData)

partitionActors :: Prop -> [Prop] -> ([Target], [Prop])
partitionActors me ps = first (map toTarget)
                      $ partition (hasActor . getTag) ps
  where
    toTarget p =
        let a = maybe (error "not an actor") id
              . view propActor
              $ getTag p
            addr = maybe (error "doesn't have an addr") id
                 . view propAddr
                 $ getTag p
            pos = center me
            occluded = (1 <)
                     . length
                     . sweepLine ps pos
                     . posDif pos
                     $ center p
         in Target a (center p) addr occluded

makeActor :: Address (Maybe Prop) -> Actor -> Signal ()
makeActor addr a = mail addr $ (_Just'.tagL.propActor .~ Just a)
                             . (_Just'.tagL.propAddr  .~ Just (addr))

_Just' :: Lens' (Maybe a) a
_Just' = lens fromJust (const Just)


{-# NOINLINE combatState #-}
{-# NOINLINE combatStateAddr #-}
combatState :: Signal CombatState
(combatState, combatStateAddr) = newMailbox "combat state" CSMenu
combatStateMsg = edges combatState

actionMenu :: Menu
actionMenu = Menu
    { _menuSelected = 0
    , _menuItems = [ MenuItem "Move"   $ setState' MovePrep
                   , MenuItem "Attack" $ setState' AttackInit
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
setState s = do
    now <- lift time
    modify $ (stateTime .~ now)
           . (state .~ s)

sinceState :: StateT CombatData Signal Time
sinceState = (-) <$> lift time <*> gets _stateTime

-- TODO(sandy): eq instance is KILLIN ME baby
setState' :: CombatState -> Signal ()
setState' = mail combatStateAddr . const

startCombat :: Signal [Prop] -> Signal Prop -> Signal ()
startCombat pss players = do
    now <- time
    mail menuAddr $ const actionMenu
    mail inputFilterAddr $ const NoneFilter
    start $ combat now pss players

combat :: Time -> Signal [Prop] -> Signal Prop -> Machine [Prop]
combat now pss players = machine (CombatData 0 0 [] CSMenu now) $ do
    lift combatStateMsg >>= \case
        Changed cs  -> setState cs
        Unchanged _ -> return ()
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
            lift $ setState' Move
        continue []

    | s == Move = do
        since <- sinceState
        when (since >= 1) $ do
            lift . mail inputFilterAddr $ const NoneFilter
            lift $ setState' CSMenu
        continue []

    | s == AttackInit = do
        let a  = view actor' player
            w  = view weapon a
            ts = sortBy (on compare (fst . unpackPos . location))
                    . filter (isTargetable w a . who)
                    . fst
                    $ partitionActors player ps
        modify $ targets .~ ts
        lift $ setState' AttackSelect
        continue []

    | s == AttackSelect = do
        st <- get
        let ts   = view targets st
            sel  = view toroidSel st
            t    = ts !! sel
            pos = location t

        whenM (lift $ keyPress RightKey) . modify $ over toroidSel (+1)
        whenM (lift $ keyPress LeftKey)  . modify $ over toroidSel (subtract 1)
        whenM (lift $ keyPress SpaceKey) $ do
            let a  = view actor' player
                w  = view weapon a
            lift $ setState' CSMenu
            lift . start . action w $ AttackParams
                { src         = a
                , targeted    = showTrace $ [t]
                , environment = ps
                }

        p <- lift . floating
                  . move (mkRel 0 (-10))
                  . teleport pos
                  $ styled red defaultLine arrow
        continue [p]

-- TODO(sandy): move this elsewhere
sword :: Int -> Weapon
sword dmg = Weapon 30 id (on (/=) _team) $ \params ->
    machine () $ do
        lift . forM_ (targeted params) $ \target -> do
            liftIO $ putStrLn "get damaged"
            mail (address target) . over (_Just'.actor.hp)
                                  $ subtract dmg
        finish []

