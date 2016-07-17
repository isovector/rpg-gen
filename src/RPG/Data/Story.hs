{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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
    , StoryRei
    , ReduceState (..)
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

data ReduceState = Constructed | Applied | Reified

type family Select s a b :: * where
    Select 'Constructed a b = a -> b
    Select 'Applied     a b = b
    Select 'Reified     a b = b

type family Ignore s a b :: * where
    Ignore 'Constructed a b = a
    Ignore 'Applied     a b = a
    Ignore 'Reified     a b = b

data StoryF s a = Change Character ChangeType (Select s ChangeResult a)
                | forall x y. Interrupt (Ignore s (Free (StoryF s) x) a)
                                        (Ignore s (Free (StoryF s) y) a)
                                        (Select s y a)
                | Macguffin (Select s Desirable a)

instance Functor (StoryF 'Applied) where
    fmap f (Change c ct k)   = Change    c ct (f k)
    fmap f (Interrupt x y k) = Interrupt x y  (f k)
    fmap f (Macguffin k)     = Macguffin      (f k)

instance Functor (StoryF 'Constructed) where
    fmap f (Change c ct k)   = Change    c ct (fmap f k)
    fmap f (Interrupt x y k) = Interrupt x y  (fmap f k)
    fmap f (Macguffin k)     = Macguffin      (fmap f k)

instance Functor (StoryF 'Reified) where
    fmap f (Change c ct k)   = Change     c     ct   (f k)
    fmap f (Interrupt x y k) = Interrupt (f x) (f y) (f k)
    fmap f (Macguffin k)     = Macguffin             (f k)

data CoStoryF k = CoStoryF
                  { changeH    :: Character -> ChangeType -> (ChangeResult, k)
                  , interruptH :: forall y x. Free (StoryF 'Constructed) x
                                           -> Free (StoryF 'Constructed) y
                                           -> (y, k)
                  , macguffinH :: (Desirable, k)
                  }

instance Functor CoStoryF where
    fmap f (CoStoryF c i m) = CoStoryF
        ((fmap . fmap . fmap) f c)
        ((fmap . fmap . fmap) f i)
        (fmap f m)

type Story = Free (StoryF 'Constructed)
type StoryApp = Free (StoryF 'Applied)
type StoryRei = Free (StoryF 'Reified)
type CoStoryT = CofreeT CoStoryF

instance Zap (StoryF 'Constructed) CoStoryF where
    zap f (Change    c ct k) (CoStoryF h _ _) = zap f k (h c ct)
    zap f (Interrupt a a' k) (CoStoryF _ h _) = zap f k (h a a')
    zap f (Macguffin      k) (CoStoryF _ _ h) = zap f k h

change :: Character -> ChangeType -> Story ChangeResult
change c ct = liftF $ Change c ct id

interrupt :: Story c -> Story y -> Story y
interrupt a y = liftF $ Interrupt a y id

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

