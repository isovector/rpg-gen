{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module RPG.Machine where

import RPG.Core hiding (lift)
import Game.Sequoia.Color
import Game.Sequoia (Time)
import Data.Proxy (Proxy (..))
import Data.Singletons.TH
import Exinst.Singletons
import Data.List (foldl')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer


----

$(singletons [d|
  data MachineType = Fireball
  |])

instance c (f 'Fireball) => Dict1 c (f :: MachineType -> k) where
    dict1 = \case
        SFireball -> Dict

data family MachineData (mtype :: MachineType) :: *

data Action = Spawn (B Prop)
            | ClearSpawns

data MachineState = Wait Time

newtype Machine a = Machine
                  { scheduler :: Coroutine (Yield MachineState) (WriterT [Action] N) a
                  } deriving (Functor, Applicative, Monad, MonadIO)

instance Sample Machine where
    sample b = Machine . lift . lift $ sample b

test = do
    clearSpawns
    liftIO $ putStrLn "yo"
    wait 5
    liftIO $ putStrLn "hi"
    return ()

tellMachine :: Action -> Machine ()
tellMachine = Machine . lift . tell . return

spawn :: B Prop -> Machine ()
spawn = tellMachine . Spawn

wait :: Time -> Machine ()
wait = Machine . yield . Wait

clearSpawns :: Machine ()
clearSpawns = tellMachine ClearSpawns


newMachineScheduler :: Clock
                    -> Now ( B Prop
                           , Machine () -> IO ()
                           )
newMachineScheduler clock = do
    (b, mb) <- foldmp [] runMachine
    return ( fmap group . join . flip fmap b $ sequence . join . fmap (view _3)
           , \a -> mb ((a, Wait 0, []) :)
           )
  where
    foo :: [B Prop] -> Action -> [B Prop]
    foo _ ClearSpawns = []
    foo xs (Spawn x)  = x : xs

    runMachine ::   [(Machine (), MachineState, [B Prop])]
               -> N [(Machine (), MachineState, [B Prop])]
    runMachine [] = return []
    runMachine ((Machine m, waiting, props) : xs)

     | Wait time <- waiting, time <= 0 = do
        (cont, actions) <- runWriterT $ resume m
        let props' = foldl' foo props actions
        case cont of
          Right ()         -> return xs
          Left (Yield w c) -> do
              return $ (Machine c, w, props') : xs

     | Wait time <- waiting = do
         dt <- sample $ deltaTime clock
         return $ (Machine m, Wait $ time - dt, props) : xs


----


data Metadata a = Metadata
                { machineData :: a
                , totalTime   :: Time
                , stateTime   :: Time
                }
