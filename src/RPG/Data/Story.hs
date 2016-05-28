{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module RPG.Data.Story
    ( StoryT
    , StoryF ()
    , CoStoryT
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

import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
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

data StoryF m a = Change Character ChangeType (ChangeResult -> a)
                | forall b. Interrupt (FreeT (StoryF m) m ()) (FreeT (StoryF m) m b) (b -> a)
                | Macguffin (Desirable -> a)

-- IDEA(sandy): Possible to make a StoryWInterruptF = LiftStoryF StoryF
-- | Interrupted, and use typeclasses to define the value-level commands so
-- that `interrupted` only becomes available inside of an `interrupt` block?

instance Functor (StoryF m) where
    fmap f (Change c ct k)   = Change c ct (f . k)
    fmap f (Interrupt a b k) = Interrupt a b (f . k)
    fmap f (Macguffin k)     = Macguffin (f . k)

data CoStoryF m k = CoStoryF
                  { changeH    :: Character -> ChangeType -> (ChangeResult, k)
                  , interruptH :: forall b. FreeT (StoryF m) m () -> FreeT (StoryF m) m b -> (b, k)
                  , macguffinH :: (Desirable, k)
                  }

instance Functor (CoStoryF m) where
    fmap f (CoStoryF c i m) = CoStoryF
        (fmap (fmap (fmap f)) c)
        (fmap (fmap (fmap f)) i)
        (fmap f m)

type StoryT m = FreeT (StoryF m) m
type CoStoryT w m = CofreeT (CoStoryF m) w

instance Pairing (CoStoryF m) (StoryF m) where
    pair f (CoStoryF t _ _) (Change c ct k)    = pair f (t c ct) k
    pair f (CoStoryF _ t _) (Interrupt a a' k) = pair f (t a a') k
    pair f (CoStoryF _ _ t) (Macguffin k)      = pair f t k

change :: Monad m => Character -> ChangeType -> StoryT m ChangeResult
change c ct = liftF $ Change c ct id

interrupt :: Monad m => StoryT m () -> StoryT m b -> StoryT m b
interrupt a b = liftF $ Interrupt a b id

macguffin :: Monad m => StoryT m Desirable
macguffin = liftF $ Macguffin id

kill :: Monad m => Character -> Character -> StoryT m ChangeResult
kill who whom = change who (Kill whom) <* die whom

die :: Monad m => Character -> StoryT m ChangeResult
die = flip change Die

want :: Monad m => Character -> Desirable -> StoryT m ChangeResult
want who = change who . Want

learnOf :: Monad m => Character -> ChangeResult -> StoryT m ChangeResult
learnOf who = change who . Learn . ChangeOf

