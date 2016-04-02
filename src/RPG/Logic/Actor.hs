{-# LANGUAGE TemplateHaskell #-}
module RPG.Logic.Actor
    ( newActor
    , allActors
    , getActor
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
    aid <- liftIO newActorId
    (sig, addr) <- liftIO $ do
        -- TODO(sandy): this maybe wants a foldmp instead
        mailbox $ Actor (error "no address!") hp mp team
    mail addr $ actorAddr .~ addr
    liftIO $ addActor aid sig
    return sig

{-# NOINLINE allActors #-}
{-# NOINLINE allActorsAddr #-}
allActors :: Signal (Map ActorId (Signal Actor))
(allActors, allActorsAddr) = newMailbox "all actors" M.empty

{-# NOINLINE addActor #-}
addActor :: ActorId -> Signal Actor -> IO ()
addActor = addX' allActorsAddr

getActor :: ActorId -> Signal Actor
getActor = join . getCurX allActors . pure

