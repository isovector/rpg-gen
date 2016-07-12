{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Monad.Free
import Data.Function (fix)
import Data.Pairing
import Data.Set (Set)
import RPG.Data.Story
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Set as S

liftCoStory :: Comonad w
            => w b
            -> (w b -> Character -> ChangeType -> (ChangeResult, w b))
            -> (forall x x' . (forall a. Story a -> (a, b))
                           -> w b
                           -> Story x'
                           -> Story x
                           -> (x, w b))
            -> (w b -> (Desirable, w b))
            -> CoStoryT w b
liftCoStory start changeH interruptH macguffinH =
    fix $ flip coiterT start . next . flip runStory
  where
    next run w =
        CoStoryF
            (changeH w)
            (interruptH (unsafeCoerce run) w)
            (macguffinH w)

mkCoStory :: CoStoryT (Store (Set Character)) (Set Character)
mkCoStory = liftCoStory (store id S.empty) changeH interruptH macguffinH
  where
    changeH w c ct = (ChangeResult c ct, seeks (S.insert c) w)
    interruptH
        (run :: forall a. Story a -> (a, Set Character))
        w a a' = ( fst $ run a'
                 , merge a . merge a' $ w
                 )
      where merge = seeks . S.union . snd . run
    macguffinH w = (Desirable "", w)

appStory :: CoStoryT (Store Int) ()
appStory = liftCoStory (store (const ()) 0) changeH interruptH macguffinH
  where
    changeH w c ct = (ChangeResult c ct, w)
    interruptH
        (run :: forall a. Story a -> (a, ()))
        w a a' = (fst $ run a', w)
    macguffinH w = (Desirable . show $ pos w, seeks (+1) w)

apply :: Int -> Story a -> StoryApp a
apply i (Free (Change c ct k)) = Free
                               . Change  c ct
                               . Snd
                               . apply i
                               . k
                               $ ChangeResult c ct
apply i (Free (Interrupt a a' k)) =
    let b = fst $ runStory a' appStory
     in   Free
        . Interrupt (apply i a) (apply i a')
        . Snd
        . apply i
        $ k b
apply i (Free (Macguffin k)) = Free
                             . Macguffin
                             . Snd
                             . apply (i + 1)
                             . k
                             . Desirable
                             $ show i
apply _ (Pure a) = Pure a

runStory :: Comonad w => Story a -> CoStoryT w b -> (a, b)
runStory = pairEffect (,)

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

