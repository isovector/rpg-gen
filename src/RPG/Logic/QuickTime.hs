{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE LambdaCase #-}
module RPG.Logic.QuickTime
    ( startQuickTime
    , fireball
    , quickTime
    ) where

import RPG.Core
import Game.Sequoia.Keyboard
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE state #-}
{-# NOINLINE stateAddr #-}
{-# NOINLINE fireballCount #-}
{-# NOINLINE fireballAddr #-}

quickTime :: Signal QuickTime
(quickTime, quickTimeAddr) = newMailbox "quicktime" fireball
(state, stateAddr) = newMailbox "state" 0
(startTime, startTimeAddr) = newMailbox "start-time" 0
(stateTime, stateTimeAddr) = newMailbox "end-time" 0

type QuickTime = Signal [Prop]

startQuickTime :: QuickTime -> Signal ()
startQuickTime qt = do
    setState 0
    mail quickTimeAddr $ const qt
    mail startTimeAddr . const =<< time

setState :: Int -> Signal ()
setState s = do
    mail stateAddr $ const s
    mail stateTimeAddr . const =<< time

sinceStarting :: Signal Time
sinceStarting = (-) <$> time <*> startTime

sinceState :: Signal Time
sinceState = (-) <$> time <*> stateTime

mashing :: Signal Int
mashing = countIf id $ keyPress SpaceKey

fireballCount :: Signal Int
(fireballCount, fireballAddr) = newMailbox "fireball" 0

fireball :: QuickTime
fireball = state >>= \case
    0 -> do
        since <- sinceState
        when (since >= 3) $ do
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
            setState 2
        when (since >= 0.5) $ do
            liftIO . putStrLn $ "pew pew" ++ show fireballs
            mail fireballAddr $ const (fireballs - 1)
            setState 2
        return []






