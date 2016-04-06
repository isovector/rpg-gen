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
    , sinceStarting
    , lift
    ) where

import Control.Arrow (first, second)
import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (liftIO)
import RPG.Core

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

into :: (StackFrame -> s) -> QuickTime a s
into l = lift $ l <$> currentStack

startTime :: QuickTime a Time
startTime = into _sfStartTime

sinceStarting :: QuickTime a Time
sinceStarting = (-) <$> lift time <*> startTime

start :: Machine [Prop] -> Signal ()
start m = do
    now <- time
    let frame = StackFrame m now now
    mail stateMachineAddr (frame :)

continue :: a -> StateT s Signal (Bool, a)
continue a = return (True, a)

finish :: a -> StateT s Signal (Bool, a)
finish a = return (False, a)

