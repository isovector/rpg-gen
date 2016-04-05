{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.QuickTime
    ( start
    , setState
    , setState'
    , finish
    , fireball
    , runQuickTime
    , sinceState
    , sinceStarting
    , lift
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State hiding (state)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Game.Sequoia.Keyboard
import RPG.Core
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

data Improved = forall a. Improved
    { _running :: Bool
    , _run  :: Improved -> Signal (Improved, [Prop])
    , _update :: StateT a Signal (Bool, [Prop])
    , _data    :: a
    }

mkImproved :: a -> StateT a Signal (Bool, [Prop]) -> Improved
mkImproved a t = Improved True run t a
  where
    run Improved{..} = do
        ((r, a'), s) <- runStateT _update _data
        return (Improved r run _update s, a')

data StackFrame a = StackFrame
    { _sfEvent :: Maybe Int -> QuickTime a [Prop]
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
    frames <- length <$> stateMachine
    if frames /= 0
       then runQuickTime'
       else return []

runQuickTime' :: Signal [Prop]
runQuickTime' = do
    frame <- unsafeCoerce currentStack
    let dat = _sfData frame
        st  = Just $ _sfState frame

    len    <- length <$> stateMachine
    (a, s) <- runStateT (_sfEvent frame st) dat
    len'   <- length <$> stateMachine

    -- TODO(sandy): there might be a bug here if you finish this frame
    when (len == len')
        . mail stateMachineAddr
        $ headLens.sfData .~ s

    return a

startTime :: QuickTime a Time
startTime = into _sfStartTime

stateTime :: QuickTime a Time
stateTime = into _sfStateTime

headLens :: Lens' [a] a
headLens = lens head (\as a -> a : tail as)


setState' :: Int -> Signal ()
setState' s = do
    now <- time
    mail stateMachineAddr
         $ (headLens.sfStateTime .~ now)
         . (headLens.sfState .~ s)

setState :: Int -> QuickTime a ()
setState = lift . setState'

start :: (Maybe Int -> QuickTime a [Prop]) -> Signal ()
start qt = do
    now <- time
    (_, dat) <- runStateT (qt Nothing) $ error "no data"
    let frame = StackFrame qt 0 now now dat
    mail stateMachineAddr (frame :)

finish :: QuickTime a ()
finish = lift $ mail stateMachineAddr tail

sinceStarting :: QuickTime a Time
sinceStarting = (-) <$> lift time <*> startTime

sinceState :: QuickTime a Time
sinceState = (-) <$> lift time <*> stateTime

mashing :: QuickTime a Int
mashing = lift . countIf id $ keyPress SpaceKey

fireball :: Maybe Int -> QuickTime Int [Prop]
fireball = \case
    Nothing -> return []
    Just 0 -> do
        since <- sinceState
        when (since >= 1) $ do
            liftIO $ putStrLn "and go!"
            setState 1
        return []

    Just 1 -> do
        mashing
        since <- sinceState
        when (since >= 2) $ do
            mashed <- mashing
            liftIO . putStrLn $ show mashed
            put mashed
            setState 2
        return []

    Just 2 -> do
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

