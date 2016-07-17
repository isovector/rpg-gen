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

type family Cx s a f b :: * where
    Cx 'Constructed a f b = f a b
    Cx 'Applied     a f b = b
    Cx 'Reified     a f b = b

type family ReifyAs s a b :: * where
    ReifyAs 'Constructed a b = a
    ReifyAs 'Applied     a b = a
    ReifyAs 'Reified     a b = b

data StoryF s a = Change Character ChangeType (Cx s ChangeResult (->) a)
                | forall x y. Interrupt (ReifyAs s (Free (StoryF s) x) a)
                                        (ReifyAs s (Free (StoryF s) y) a)
                                        (Cx s y (->) a)
                | Macguffin (Cx s Desirable (->) a)

instance Functor (StoryF 'Constructed) where
    fmap f (Change c ct k)   = Change    c ct (fmap f k)
    fmap f (Interrupt x y k) = Interrupt x y  (fmap f k)
    fmap f (Macguffin k)     = Macguffin      (fmap f k)

instance Functor (StoryF 'Applied) where
    fmap f (Change c ct k)   = Change    c ct (f k)
    fmap f (Interrupt x y k) = Interrupt x y  (f k)
    fmap f (Macguffin k)     = Macguffin      (f k)

instance Functor (StoryF 'Reified) where
    fmap f (Change c ct k)   = Change     c     ct   (f k)
    fmap f (Interrupt x y k) = Interrupt (f x) (f y) (f k)
    fmap f (Macguffin k)     = Macguffin             (f k)

data CoStoryF s k = CoStoryF
                  { changeH    :: Character -> ChangeType -> Cx s ChangeResult (,) k
                  , interruptH :: forall x y. Free (StoryF s) x
                                           -> Free (StoryF s) y
                                           -> Cx s y (,) k
                  , macguffinH :: Cx s Desirable (,) k
                  }

instance Functor (CoStoryF 'Constructed) where
    fmap f (CoStoryF c i m) = CoStoryF
        ((fmap . fmap . fmap) f c)
        ((fmap . fmap . fmap) f i)
        (fmap f m)

type Story = Free (StoryF 'Constructed)
type StoryApp = Free (StoryF 'Applied)
type StoryRei = Free (StoryF 'Reified)
type CoStoryT = CofreeT (CoStoryF 'Constructed)
type CoStoryAppT = CofreeT (CoStoryF 'Applied)

instance Zap (StoryF 'Constructed) (CoStoryF 'Constructed) where
    zap f (Change    c ct k) (CoStoryF h _ _) = zap f k (h c ct)
    zap f (Interrupt x y  k) (CoStoryF _ h _) = zap f k (h x y)
    zap f (Macguffin      k) (CoStoryF _ _ h) = zap f k h

instance Zap (StoryF 'Applied) (CoStoryF 'Applied) where
    zap f (Change    c ct k) (CoStoryF h _ _) = f k (h c ct)
    zap f (Interrupt x y  k) (CoStoryF _ h _) = f k (h x y)
    zap f (Macguffin      k) (CoStoryF _ _ h) = f k h

change :: Character -> ChangeType -> Story ChangeResult
change c ct = liftF $ Change c ct id

interrupt :: Story x -> Story y -> Story y
interrupt x y = liftF $ Interrupt x y id

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

