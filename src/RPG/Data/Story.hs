{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module RPG.Data.Story
    ( Story
    , StoryF (..)
    , CoStory
    , CoStoryF (..)
    , ChangeType (..)
    , ChangeResult (..)
    , Knowledge (..)
    , Opinion (..)
    , Character (..)
    , Desirable (..)
    , change
    , interrupt
    , macguffin
    , kill
    , die
    , want
    , learnOf
    ) where

import Control.Monad.Free
import Control.Comonad.Cofree
import Control.Monad.Free.TH
import Data.Pairing

data Desirable = Desirable String deriving (Eq)
instance Show Desirable where
    show (Desirable name) = name

data Character = Character String deriving (Eq)
instance Show Character where
    show (Character name) = name

data Opinion = Friend
             | Neutral
             | Enemy
             deriving (Eq, Show)

data Knowledge = ChangeOf ChangeResult deriving (Eq, Show)

data ChangeResult = ChangeResult Character ChangeType
    deriving (Eq, Show)
data ChangeType = Introduce
                | Die
                | Kill Character
                | Leave
                | Arrive
                | Learn Knowledge
                | Obtain
                | Want Desirable
                | Achieve Desirable
                | Feel Character Opinion
                deriving (Eq, Show)

data StoryF a = Change Character ChangeType (ChangeResult -> a)
              | forall b. Interrupt (Story ()) (Story b) (b -> a)
              | Macguffin (Desirable -> a)

-- IDEA(sandy): Possible to make a StoryWInterruptF = LiftStoryF StoryF
-- | Interrupted, and use typeclasses to define the value-level commands so
-- that `interrupted` only becomes available inside of an `interrupt` block?

instance Functor StoryF where
    fmap f (Change c ct k)   = Change c ct (f . k)
    fmap f (Interrupt a b k) = Interrupt a b (f . k)
    fmap f (Macguffin k)     = Macguffin (f . k)

change :: Character -> ChangeType -> Story ChangeResult
change c ct = liftF $ Change c ct id

interrupt :: Story () -> Story b -> Story b
interrupt a b = liftF $ Interrupt a b id

macguffin :: Story Desirable
macguffin = liftF $ Macguffin id

data CoStoryF k = CoStoryF
                { changeH    :: Character -> ChangeType -> (ChangeResult, k)
                , interruptH :: forall b. Story () -> Story b -> (b, k)
                , macguffinH :: (Desirable, k)
                }

instance Functor CoStoryF where
    fmap f (CoStoryF c i m) = CoStoryF
        (fmap (fmap (fmap f)) c)
        (fmap (fmap (fmap f)) i)
        (fmap f m)

type Story   = Free   StoryF
type CoStory = Cofree CoStoryF

instance Pairing CoStoryF StoryF where
    pair f (CoStoryF t _ _) (Change c ct k)    = pair f (t c ct) k
    pair f (CoStoryF _ t _) (Interrupt a a' k) = pair f (t a a') k
    pair f (CoStoryF _ _ t) (Macguffin k)      = pair f t k

kill :: Character -> Character -> Story ChangeResult
kill who whom = change who (Kill whom) <* die whom

die :: Character -> Story ChangeResult
die = flip change Die

want :: Character -> Desirable -> Story ChangeResult
want who = change who . Want

learnOf :: Character -> ChangeResult -> Story ChangeResult
learnOf who = change who . Learn . ChangeOf

