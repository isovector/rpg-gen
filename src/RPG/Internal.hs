{-# LANGUAGE RankNTypes #-}
module RPG.Internal
    ( Prop
    , Loc (..)
    , Interaction (..)
    , Target (..)
    , Team
    , QuickTime
    , Attack
    , AttackParams (..)
    , Weapon (..)
    , Actor (..)
    , Tag (..)
    ) where

import Control.Monad.State hiding (state)
import Data.Default
import Data.Function (on)
import Game.Sequoia

type Prop = Prop' Tag

newtype Loc = Loc Int
    deriving (Eq, Show, Ord)

data Interaction = Teleport Loc Int
    deriving (Eq, Show)

data Target = Target
    { who :: Actor
    , location :: Pos
    , address :: Address Prop
    , isOccluded :: Bool
    }

instance Show Target where
    show t = (show $ location t) ++ (show $ isOccluded t)

type Team = Int

type QuickTime s a = StateT s Signal a

type Attack a = AttackParams -> Int -> QuickTime a ()

data AttackParams = AttackParams
    { src :: Actor
    , targeted :: [Target]
    , environment :: [Prop]
    }

data Weapon a = Weapon
    { range :: Double
    , cost :: Actor -> Actor
    , isTargetable :: Actor -> Actor -> Bool
    , action :: Attack a
    }

data Actor = Actor
    { _hp :: Int
    -- , maxHp :: Int
    , _mp :: Int
    -- , maxMp :: Int
    , _team :: Team
    , _weapon :: forall a. Weapon a
    }

instance Eq Actor where
    a1 == a2 = let f g = on (==) g a1 a2
                in all id [ f _hp
                          , f _mp
                          , f _team
                          ]

data Tag = Tag
    { _hasCollision :: Bool
    , _isFloor      :: Bool
    , _propKey      :: Maybe Int
    , _interaction  :: Maybe Interaction
    , _propAddr     :: Maybe (Address Prop)
    , _propActor    :: Maybe Actor
    }

instance Eq Tag where
    t1 == t2 = let f g = on (==) g t1 t2
                in all id [ f _hasCollision
                          , f _isFloor
                          , f _propKey
                          , f _propActor
                          ]

instance Default Tag where
    def = Tag False False Nothing Nothing Nothing Nothing

