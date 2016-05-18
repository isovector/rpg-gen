{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.Gen.Portal
    ( portal
    ) where

import Data.IORef
import Game.Sequoia.Color (yellow)
import Game.Sequoia.Utils
import RPG.Core
import RPG.Scene

portal :: ( Some r
          , Has (Loc -> IO ()) r
          , Has (Int -> Prop -> IO ()) r
          , Has (Int -> B (Maybe Prop)) r
          , Has (IORef ((Prop -> Prop) -> IO ())) r
          )
       => Loc
       -> Loc
       -> Eff r (Prop, Prop)
portal dst1 dst2 = do
    (setLoc       :: Loc -> IO ())            <- ask
    (findProp     :: Int -> B (Maybe Prop))   <- ask
    (registerProp :: Int -> Prop -> IO ())    <- ask
    (movePlayerIO :: IORef ((Prop -> Prop) -> IO ())) <- ask
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey $ getTag p1
        id2 = maybe undefined id . view propKey $ getTag p2
        f d i = tags . set interaction . Just $ do
            pos <- fmap (maybe origin center) . sample $ findProp i
            liftIO $ do
                movePlayer <- readIORef movePlayerIO
                setLoc d
                movePlayer $ teleport pos

    liftIO $ do
        registerProp id1 p1
        registerProp id2 p2

    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

portalGen :: Some r => Eff r Prop
portalGen = do
    idkey <- Just <$> int
    return . tags (propKey .~ idkey)
           . traced yellow
           $ rect origin 40 40

