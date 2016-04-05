{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module RPG.Logic.QuickTime
    ( Machine
    , start
    , continue
    , finish
    , runQuickTime
    , sinceState
    , sinceStarting
    , lift
    , fireball
    ) where

import Control.Arrow (first, second)
import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Game.Sequoia.Keyboard
import RPG.Core
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

data StackFrame = StackFrame
    { _sfImproved  :: Machine [Prop]
    , _sfStartTime :: Time
    , _sfStateTime :: Time
    }
$(makeLenses ''StackFrame)


{-# NOINLINE stateMachine #-}
{-# NOINLINE stateMachineAddr #-}
stateMachine :: Signal [StackFrame]
(stateMachine, stateMachineAddr) = newMailbox "state machine" []

currentStack :: Signal StackFrame
currentStack = fmap head stateMachine

runQuickTime :: Signal [Prop]
runQuickTime = do
    frames <- length <$> stateMachine
    if frames /= 0
       then runQuickTime'
       else return []

runQuickTime' :: Signal [Prop]
runQuickTime' = _sfImproved <$> currentStack >>= \case
    Machine{_run = run} -> do
        (mayImp', a) <- run
        mail stateMachineAddr $
            case mayImp' of
              Just imp' -> headL.sfImproved .~ imp'
              Nothing   -> tail
        return a

headL :: Lens' [a] a
headL = lens head (\as a -> a : tail as)

into :: (StackFrame -> s) -> QuickTime a s
into l = lift $ l <$> currentStack

startTime :: QuickTime a Time
startTime = into _sfStartTime

stateTime :: QuickTime a Time
stateTime = into _sfStateTime

sinceStarting :: QuickTime a Time
sinceStarting = (-) <$> lift time <*> startTime

sinceState :: QuickTime a Time
sinceState = (-) <$> lift time <*> stateTime

start :: Machine [Prop] -> Signal ()
start m = do
    now <- time
    let frame = StackFrame m now now
    mail stateMachineAddr (frame :)

continue :: a -> StateT s Signal (Bool, a)
continue a = return (True, a)

finish :: a -> StateT s Signal (Bool, a)
finish a = return (False, a)

mashing :: QuickTime a Int
mashing = lift . countIf id $ keyPress SpaceKey

fireball :: Machine [Prop]
fireball = machine (0, 0) $ fst <$> get >>= \case
    0 -> do
        since <- sinceState
        when (since >= 1) $ do
            liftIO $ putStrLn "and go!"
            modify . first $ const 1
        continue []

    1 -> do
        mashing
        since <- sinceState
        when (since >= 2) $ do
            mashed <- mashing
            liftIO . putStrLn $ show mashed
            modify . second $ const mashed
            modify . first $ const 2
        continue []

    2 -> do
        since <- sinceState
        fireballs <- snd <$> get
        when (since >= 0.5) $ do
            liftIO . putStrLn $ "pew pew" ++ show fireballs
            modify . first $ subtract 1
            modify . first $ const 2

        if fireballs == 0
           then finish []
           else continue []

