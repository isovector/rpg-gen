{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module RPG.Data.Story
    ( Story
    , StoryF (..)
    , CoStoryT
    , CoStoryF (..)
    , ChangeType (..)
    , ChangeResult (..)
    , Knowledge (..)
    , Opinion (..)
    , Character (..)
    , Desirable (..)
    , Snd (..)
    , StoryApp
    , change
    , interrupt
    , macguffin
    , kill
    , die
    , want
    , learnOf
    ) where

import Control.Monad.Free
import Control.Comonad.Trans.Cofree
import Data.Pairing


newtype Snd f g = Snd { runSnd :: g } deriving Functor

type family Apply g a b :: * where
    Apply (->) a b = a -> b
    Apply Snd  a b = b

data Desirable = Desirable String deriving (Eq, Ord)
instance Show Desirable where
    show (Desirable name) = name

data Character = Character String deriving (Eq, Ord)
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

data StoryF g a = Change Character ChangeType (Apply g ChangeResult a)
                | forall x x'. Interrupt (Free (StoryF g) x')
                                         (Free (StoryF g) x)
                                         (Apply g x a)
                | Macguffin (Apply g Desirable a)

instance Functor (StoryF Snd) where
    fmap f (Change c ct k)   = Change    c ct (f k)
    fmap f (Interrupt a x k) = Interrupt a x  (f k)
    fmap f (Macguffin k)     = Macguffin      (f k)

instance Functor (StoryF (->)) where
    fmap f (Change c ct k)   = Change    c ct (fmap f k)
    fmap f (Interrupt a x k) = Interrupt a  x (fmap f k)
    fmap f (Macguffin k)     = Macguffin      (fmap f k)

data CoStoryF k = CoStoryF
                  { changeH    :: Character -> ChangeType -> (ChangeResult, k)
                  , interruptH :: forall x x'. Free (StoryF (->)) x'
                                            -> Free (StoryF (->)) x
                                            -> (x, k)
                  , macguffinH :: (Desirable, k)
                  }

instance Functor CoStoryF where
    fmap f (CoStoryF c i m) = CoStoryF
        ((fmap . fmap . fmap) f c)
        ((fmap . fmap . fmap) f i)
        (fmap f m)

type Story = Free (StoryF (->))
type StoryApp = Free (StoryF Snd)
type CoStoryT = CofreeT CoStoryF

instance Zap (StoryF (->)) CoStoryF where
    zap f (Change    c ct k) (CoStoryF h _ _) = zap f k (h c ct)
    zap f (Interrupt a a' k) (CoStoryF _ h _) = zap f k (h a a')
    zap f (Macguffin      k) (CoStoryF _ _ h) = zap f k h

change :: Character -> ChangeType -> Story ChangeResult
change c ct = liftF $ Change c ct id

interrupt :: Story c -> Story x -> Story x
interrupt a x = liftF $ Interrupt a x id

macguffin :: Story Desirable
macguffin = liftF $ Macguffin id

kill :: Character -> Character -> Story ChangeResult
kill who whom = change who (Kill whom) <* die whom

die :: Character -> Story ChangeResult
die = flip change Die

want :: Character -> Desirable -> Story ChangeResult
want who = change who . Want

learnOf :: Character -> ChangeResult -> Story ChangeResult
learnOf who = change who . Learn . ChangeOf

