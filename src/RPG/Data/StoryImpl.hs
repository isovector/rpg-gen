{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

liftCoStory :: Comonad w
            => w b
            -> (forall a. (Story a -> (a, b)) -> w b -> CoStoryF (w b))
            -> CoStoryT w b
liftCoStory start next = fix $ flip coiterT start . next . runStory

mkCoStory :: CoStoryT (Store (Set Character)) (Set Character)
mkCoStory = liftCoStory (store id S.empty) next
  where
    next run w = CoStoryF (coChange w) (coInterrupt (unsafeCoerce run) w) (coMacguffin w)

    coChange w c ct = (ChangeResult c ct, seeks (S.insert c) w)
    coInterrupt
        (run :: forall a. Story a -> (a, Set Character))
        w a a' = ( fst $ run a'
                 , merge a . merge a' $ w
                 )
      where merge = seeks . S.union . snd . run
    coMacguffin w = (Desirable "", w)

runStory :: Comonad w => CoStoryT w b -> Story a -> (a, b)
runStory = pairEffect $ flip (,)

dopestory :: Story Character
dopestory = do
    let johnny = Character "Mr. Monkey"
    let crab = Character "The Lord of Crabs"
    let scrub = Character "Jared"

    void . change johnny $ Feel scrub Friend
    thing <- macguffin
    want crab thing
    want scrub thing

    ChangeResult who _ <- interrupt hypothetical $ do
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

