{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.StoryImpl
    ( runStory
    , dopestory
    , mkCoStory
    , scata
    , rcata
    , characters
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
            -> (forall x y . (forall a. Story a -> (a, b))
                          -> w b
                          -> Story x
                          -> Story y
                          -> (y, w b))
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
                               . Change c ct
                               . apply i
                               . k
                               $ ChangeResult c ct
apply i (Free (Interrupt a a' k)) =
    let b = fst $ runStory a' appStory
     in   Free
        . Interrupt (apply i a) (apply i a')
        . apply i
        $ k b
apply i (Free (Macguffin k)) = Free
                             . Macguffin
                             . apply (i + 1)
                             . k
                             . Desirable
                             $ show i
apply _ (Pure a) = Pure a

reify :: forall a b. a -> StoryApp b -> StoryRei a
reify a = fcata alg (const $ Pure a)
  where
    alg :: Algebra (StoryF 'Applied) (StoryRei a)
    alg (Change c ct k)   = Free $ Change c ct k
    alg (Interrupt x y k) = Free $ Interrupt (reify a x) (reify a y) k
    alg (Macguffin k)     = Free $ Macguffin k

type Algebra f a = f a -> a
newtype Fix f = Iso { invIso :: f (Fix f) }

fcata :: Functor f => Algebra f a -> (b -> a) -> Free f b -> a
fcata alg f (Pure b) = f b
fcata alg f (Free free) = alg . fmap (fcata alg f) $ free

scata :: Algebra (StoryF 'Applied) a -> (b -> a) -> Story b -> a
scata alg f = fcata alg f . apply 0

rcata :: Algebra (StoryF 'Reified) a -> a -> Story b -> a
rcata alg a = fcata alg (const a) . reify a . apply 0

characters :: Algebra (StoryF 'Reified) (Set Character)
characters (Change c (Kill c')   cs) = S.insert c $ S.insert c' cs
characters (Change c (Feel c' _) cs) = S.insert c $ S.insert c' cs
characters (Change c _ cs) = S.insert c cs
characters (Interrupt as bs cs) = mconcat [as, bs, cs]
characters (Macguffin cs) = cs


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

