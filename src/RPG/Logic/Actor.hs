{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Actor
    (
    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import RPG.Core
import RPG.Logic.Utils
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

{-# NOINLINE actorIdGen #-}
actorIdGen :: IORef ActorId
actorIdGen = unsafePerformIO $ newIORef (ActorId 0)

newActorId :: IO ActorId
newActorId = newX (\(ActorId i) -> ActorId $ i + 1) actorIdGen

newActor :: Int -- ^Max HP
         -> Int -- ^Max MP
         -> Team
         -> Signal (Signal Actor)
newActor hp mp team = do
    (sig, addr) <- liftIO $ do
        aid <- newActorId
        mailbox $ Actor aid (error "no address!") hp mp team
    mail addr $ actorAddr .~ addr
    return sig

{-# NOINLINE allActors #-}
{-# NOINLINE allActorsAddr #-}
allActors :: Signal (Map ActorId (Signal Actor))
(allActors, allActorsAddr) = newMailbox "all actors" M.empty

{-# NOINLINE addActor #-}
addActor :: Loc -> Signal [Prop] -> IO ()
addActor = addX' allActorsAddr

