{-# LANGUAGE FlexibleContexts #-}

module RPG.Data.StoryImpl
    ( runStory
    , dopestory
    , mkCoStory
    ) where

import Control.Comonad
import Control.Comonad.Store
import Control.Comonad.Trans.Cofree
import Control.Monad (void)
import Control.Monad.Trans.Free
import Data.Function (fix)
import Data.Pairing
import Data.Set (Set)
import RPG.Data.Story
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Set as S

infixl 1 =>>>
(=>>>) :: Comonad w => w a -> (w a -> w b) -> w b
a =>>> f = a =>> extract . f

mkCoStory :: CoStoryT (Store (Set Character)) (Set Character)
mkCoStory = fix $ \me -> coiterT (next $ runStory me) start
  where
    next run w = CoStoryF (coChange w) (unsafeCoerce . coInterrupt run w) (coMacguffin w)

    start :: Store (Set Character) (Set Character)
    start = store id S.empty

    coChange w c ct        = (ChangeResult c ct, seeks (S.insert c) w)
    coInterrupt run w a a' = ( snd $ run a'
                             , w =>>> merge a
                                 =>>> merge a'
                             )
      where merge s = seeks (S.union . fst $ run s)
    coMacguffin w          = (Desirable "", w)

pairEffect :: (Pairing f g, Comonad w, Functor f, Functor g)
           => (a -> b -> r)
           -> CofreeT f w a
           -> Free g b
           -> r
pairEffect p s c = case runFree c of
                     Pure x -> p (extract s) x
                     Free gs -> pair (pairEffect p) (unwrap s) gs

runStory :: Comonad w => CoStoryT w a -> Story b -> (a, b)
runStory = pairEffect (,)

execStory :: Comonad w => CoStoryT w a -> Story b -> a
execStory = (fst .) . runStory

evalStory :: Comonad w => CoStoryT w a -> Story b -> b
evalStory = (snd .) . runStory

dopestory :: Story Character
dopestory = do
    let johnny = Character "Mr. Monkey"
    let crab = Character "The Lord of Crabs"
    let scrub = Character "Jared"

    void . change johnny $ Feel scrub Friend
    thing <- macguffin
    want crab thing
    want scrub thing

    ChangeResult who _ <- interrupt (void hypothetical) $ do
        let brancher = Character "Brancher"
        uh_oh <- kill crab scrub
        change johnny . Learn $ ChangeOf uh_oh
        change brancher $ Feel crab Enemy

    change crab Leave
    change johnny Leave
    kill johnny crab
    thing2 <- macguffin
    return who
  where
    hypothetical = do
        let hypo = Character "Hippo"
        change hypo Leave

