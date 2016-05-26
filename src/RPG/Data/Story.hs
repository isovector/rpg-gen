{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module RPG.Data.Story
    ( Story
    , StoryF (..)
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
import Control.Monad.Free.TH

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
              | Interrupt (Free StoryF ()) (Free StoryF ()) a
              | Macguffin (Desirable -> a)
              deriving Functor

$(makeFree ''StoryF)

type Story = Free StoryF

kill :: Character -> Character -> Story ChangeResult
kill who whom = change who (Kill whom) <* die whom

die :: Character -> Story ChangeResult
die = flip change Die

want :: Character -> Desirable -> Story ChangeResult
want who = change who . Want

learnOf :: Character -> ChangeResult -> Story ChangeResult
learnOf who = change who . Learn . ChangeOf

