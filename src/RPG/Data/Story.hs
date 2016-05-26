{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Data.Story
    ( change
    ) where

import Control.Monad.Free
import Control.Monad.Free.TH

data Desirable = Desirable Int
data Character = Character
data Opinion = Friend
             | Neutral
             | Enemy

data Knowledge = ChangeOf ChangeResult

data ChangeResult = ChangeResult
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

data StoryF a = Change Character ChangeType (ChangeResult -> a)
              | Interrupt (Free StoryF ()) (Free StoryF ()) a
              | Macguffin (Desirable -> a)
              deriving Functor

$(makeFree ''StoryF)

type Story = Free StoryF

kill :: Character -> Character -> Story ChangeResult
kill who whom = do
    die whom
    change who $ Kill whom

die :: Character -> Story ChangeResult
die = flip change Die

want :: Character -> Desirable -> Story ChangeResult
want who = change who . Want

learnOf :: Character -> ChangeResult -> Story ChangeResult
learnOf who = change who . Learn . ChangeOf

