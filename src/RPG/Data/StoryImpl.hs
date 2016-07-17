{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.StoryImpl
    ( runStory
    , dopestory
    , acata
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

mkCoStory :: ( Comonad w
             , Functor (StoryF s)
             , Functor (CoStoryF s)
             , Zap (StoryF s) (CoStoryF s)
             )
          => w b
          -> (w b -> Character -> ChangeType -> Cx s ChangeResult (,) (w b))
          -> (forall x y . (forall a. Free (StoryF s) a -> Cx s a (,) b)
                        -> w b
                        -> Free (StoryF s) x
                        -> Free (StoryF s) y
                        -> Cx s y (,) (w b))
          -> (w b -> Cx s Desirable (,) (w b))
          -> CofreeT (CoStoryF s) w b
mkCoStory start changeH interruptH macguffinH =
    fix $ flip coiterT start . next . flip runStory
  where
    next run w =
        CoStoryF
            (changeH w)
            (interruptH (unsafeCoerce run) w)
            (macguffinH w)

apply :: Story a -> StoryApp a
apply = (\(_,_,a) -> a) . go 0
  where
    go :: Int -> Story a -> (Int, a, StoryApp a)
    go i (Free (Change c ct k)) =
        let (i', a, k') = go i . k $ ChangeResult c ct
        in (i', a, Free $ Change c ct k')
    go i (Free (Interrupt x y k)) =
        let (i',   _, x') = go i  x
            (i'',  r, y') = go i' y
            (i''', a, k') = go i'' $ k r
        in (i''', a, Free $ Interrupt x' y' k')
    go i (Free (Macguffin k)) = go (i + 1) . k . Desirable $ show i
    go i (Pure a) = (i, a, Pure a)

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

acata :: Algebra (StoryF 'Applied) a -> (b -> a) -> Story b -> a
acata alg f = fcata alg f . apply

rcata :: Algebra (StoryF 'Reified) a -> a -> Story b -> a
rcata alg a = fcata alg (const a) . reify a . apply

characters :: Algebra (StoryF 'Reified) (Set Character)
characters (Change c (Kill c')   cs) = S.insert c $ S.insert c' cs
characters (Change c (Feel c' _) cs) = S.insert c $ S.insert c' cs
characters (Change c _ cs) = S.insert c cs
characters (Interrupt as bs cs) = mconcat [as, bs, cs]
characters (Macguffin cs) = cs

injecting :: Monad m => m c -> (a -> m b) -> a -> m b
injecting mc f a = mc >> return a >>= f

injectAfter :: Story b -> Story a -> Story a
injectAfter sb (Free (Change c ct k))   = Free . Change c ct   $ injecting sb k
injectAfter sb (Free (Interrupt x y k)) = Free . Interrupt x y $ injecting sb k
injectAfter sb (Free (Macguffin k))     = Free . Macguffin     $ injecting sb k
injectAfter sb (Pure a)                 = sb >> return a


runStory :: ( Comonad w
            , Functor (StoryF s)
            , Functor (CoStoryF s)
            , Zap (StoryF s) (CoStoryF s)
            )
         => Free (StoryF s) a
         -> CofreeT (CoStoryF s) w b
         -> (a, b)
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

