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
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import RPG.Core
import RPG.Logic.QuickTime

data Actor = Actor
    { prop :: Signal Prop
    , _address :: Address Actor
    , _hp :: Int
    -- , maxHp :: Int
    , _mp :: Int
    -- , maxMp :: Int
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
    , isOccluded :: Bool
    }

data AttackParams = AttackParams
    { src :: Actor
    , targets :: [Target]
    , environment :: [Prop]
    }

newActor :: Signal Prop
         -> Int
         -> Int
         -> Int
         -> Signal (Signal Actor)
newActor prop hp mp team = do
    (sig, addr) <- liftIO . mailbox
                          $ Actor prop (error "no address!") hp mp team
    mail addr $ address .~ addr
    return sig

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
            mail (view address $ who target) $ over hp (subtract dmg)
        finish

