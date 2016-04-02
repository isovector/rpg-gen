{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.QuickTime
    ( start
    , setState
    , finish
    , fireball
    , runQuickTime
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (liftIO)
import Game.Sequoia.Keyboard
import RPG.Core
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

type QuickTime s a = StateT s Signal a

data StackFrame a = StackFrame
    { _sfEvent :: QuickTime a [Prop]
    , _sfState :: Int
    , _sfStartTime :: Time
    , _sfStateTime :: Time
    , _sfData :: a
    }
$(makeLenses ''StackFrame)


{-# NOINLINE stateMachine #-}
{-# NOINLINE stateMachineAddr #-}
stateMachine :: Signal [forall a. StackFrame a]
(stateMachine, stateMachineAddr) = newMailbox "state machine" []

currentStack :: Signal (forall a. StackFrame a)
currentStack = fmap head stateMachine

into :: ((forall a. StackFrame a) -> b) -> QuickTime a b
into l = lift $ l <$> currentStack

runQuickTime :: Signal [Prop]
runQuickTime = do
    frame <- unsafeCoerce currentStack
    let dat = _sfData frame

    len    <- length <$> stateMachine
    (a, s) <- runStateT (_sfEvent frame) dat
    len'   <- length <$> stateMachine

    -- TODO(sandy): there might be a bug here if you finish this frame
    when (len == len')
        . mail stateMachineAddr
        $ headLens.sfData .~ s

    return a

state :: QuickTime a Int
state = into _sfState

startTime :: QuickTime a Time
startTime = into _sfStartTime

stateTime :: QuickTime a Time
stateTime = into _sfStateTime

headLens :: Lens' [a] a
headLens = lens head (\as a -> a : tail as)

setState :: Int -> QuickTime a ()
setState s = do
    now <- lift time
    lift . mail stateMachineAddr
         $ (headLens.sfStateTime .~ now)
         . (headLens.sfState .~ s)

start :: QuickTime a [Prop] -> Signal ()
start qt = do
    now <- time
    mail stateMachineAddr
        (StackFrame qt 0 now now (error "no data set on quicktime") :)

finish :: QuickTime a ()
finish = lift $ mail stateMachineAddr tail

sinceStarting :: QuickTime a Time
sinceStarting = (-) <$> lift time <*> startTime

sinceState :: QuickTime a Time
sinceState = (-) <$> lift time <*> stateTime

mashing :: QuickTime a Int
mashing = lift . countIf id $ keyPress SpaceKey

fireball :: QuickTime Int [Prop]
fireball = state >>= \case
    0 -> do
        since <- sinceState
        when (since >= 1) $ do
            liftIO $ putStrLn "and go!"
            setState 1
        return []
    1 -> do
        mashing
        since <- sinceState
        when (since >= 2) $ do
            mashed <- mashing
            liftIO . putStrLn $ show mashed
            put mashed
            setState 2
        return []
    2 -> do
        since <- sinceState
        fireballs <- get
        when (fireballs == 0) $ do
            liftIO $ putStrLn "done"
            finish
        when (since >= 0.5) $ do
            liftIO . putStrLn $ "pew pew" ++ show fireballs
            modify $ subtract 1
            setState 2
        return []

