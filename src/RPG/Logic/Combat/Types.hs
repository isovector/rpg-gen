{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Combat.Types
    ( Effects (..)
    , Actor (..)
    , hp
    , mp
    , team
    , Target (..)
    , AttackParams (..)
    , Weapon (..)
    , Attack
    , runEffects
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Function (on)
import RPG.Core
import RPG.Logic.QuickTime

data Actor = Actor
    { prop :: Prop
    , _hp :: Int
    , _mp :: Int
    -- TODO(sandy): what type should this be?
    , _team :: Int
    }
$(makeLenses ''Actor)

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
    , address :: Address Actor
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
        forM_ (targets params) $ \target -> do
            lift . mail (address target) $ over hp (subtract dmg)
        finish
