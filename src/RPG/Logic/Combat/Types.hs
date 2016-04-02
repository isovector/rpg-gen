{-# LANGUAGE LambdaCase #-}
module RPG.Logic.Combat.Types
    ( Effects (..)
    , Target (..)
    , AttackParams (..)
    , Weapon (..)
    , Attack
    , runEffects
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import RPG.Core
import RPG.Logic.Actor
import RPG.Logic.QuickTime
import qualified Data.Map as M

data Weapon a = Weapon
    { range :: Double
    , cost :: Actor -> Actor
    , isTargetable :: Actor -> Actor -> Bool
    , action :: Attack a
    }

data Effects = Effects
    { managedProps :: Signal [Prop]
    , stillRunning :: Signal Bool
    }

data Target = Target
    { who :: Actor
    , isOccluded :: Bool
    }

data AttackParams = AttackParams
    { src :: Actor
    , targets :: [Target]
    , environment :: [Prop]
    }

-- TODO(sandy): maybe split this up to run in Signal?
runEffects :: Int -> Effects -> QuickTime a [Prop]
runEffects s e = do
    running <- lift $ stillRunning e
    if running
       then lift $ managedProps e
       else setState s >> return []

type Attack a = AttackParams -> Int -> QuickTime a ()

sword :: Int -> Weapon ()
sword dmg = Weapon 30 id (on (/=) _team) $ \params -> \case
    0 -> do
        lift . forM_ (targets params) $ \target ->
            mail (view actorAddr $ who target) $ over hp (subtract dmg)
        finish

