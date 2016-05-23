{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.Gen.Portal
    ( portal
    ) where

import Game.Sequoia.Color (yellow)
import Game.Sequoia.Utils
import RPG.Core
import RPG.Scene

portal :: ( Some r
          , Has (Loc -> IO ()) r
          , Has (Int -> Prop -> IO ()) r
          , Has (Int -> B (Maybe Prop)) r
          , Has ((Prop -> Prop) -> IO ()) r
          )
       => Loc
       -> Loc
       -> Eff r (Prop, Prop)
portal dst1 dst2 = do
    (setLoc       :: Loc -> IO ())            <- ask
    (findProp     :: Int -> B (Maybe Prop))   <- ask
    (registerProp :: Int -> Prop -> IO ())    <- ask
    (movePlayer   :: (Prop -> Prop) -> IO ()) <- ask
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey $ getTag p1
        id2 = maybe undefined id . view propKey $ getTag p2
        f d i = tags . set interaction . Just $ do
            pos <- fmap (maybe origin center) . sample $ findProp i
            liftIO $ do
                setLoc d
                movePlayer $ teleport pos

    liftIO $ do
        -- BUG(sandy): these are Prop not B Prop, so their position never
        -- changes
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

