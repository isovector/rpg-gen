{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.QuickTime
    ( start
    , setState
    , finish
    , fireball
    , quicktime
    ) where

import Control.Lens
import Control.Lens.TH
import RPG.Core
import Game.Sequoia.Keyboard
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

type QuickTime = Signal [Prop]

data StackFrame = StackFrame
    { _sfEvent :: QuickTime
    , _sfState :: Int
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

quicktime :: Signal QuickTime
quicktime = view sfEvent <$> currentStack

state :: Signal Int
state = view sfState <$> currentStack

startTime :: Signal Time
startTime = view sfStartTime <$> currentStack

stateTime :: Signal Time
stateTime = view sfStateTime <$> currentStack

headLens :: Lens' [a] a
headLens = lens head (\as a -> a : tail as)

setState :: Int -> Signal ()
setState s = do
    now <- time
    mail stateMachineAddr
        $ (headLens.sfStateTime .~ now)
        . (headLens.sfState .~ s)

start :: QuickTime -> Signal ()
start qt = do
    now <- time
    mail stateMachineAddr (StackFrame qt 0 now now :)

finish :: Signal ()
finish = mail stateMachineAddr tail

sinceStarting :: Signal Time
sinceStarting = (-) <$> time <*> startTime

sinceState :: Signal Time
sinceState = (-) <$> time <*> stateTime

mashing :: Signal Int
mashing = countIf id $ keyPress SpaceKey

{-# NOINLINE fireballCount #-}
{-# NOINLINE fireballAddr #-}
fireballCount :: Signal Int
(fireballCount, fireballAddr) = newMailbox "fireball" 0

fireball :: QuickTime
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
            mail fireballAddr $ const mashed
            setState 2
        return []
    2 -> do
        since <- sinceState
        fireballs <- fireballCount
        when (fireballs == 0) $ do
            liftIO $ putStrLn "done"
            finish
        when (since >= 0.5) $ do
            liftIO . putStrLn $ "pew pew" ++ show fireballs
            mail fireballAddr $ const (fireballs - 1)
            setState 2
        return []

