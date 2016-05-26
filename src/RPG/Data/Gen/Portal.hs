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
          , Has ((Prop -> Prop) -> IO ()) r
          , Has (Loc -> PropId -> B (Maybe Prop)) r
          )
       => Loc
       -> Loc
       -> Eff r (Prop, Prop)
portal dst1 dst2 = do
    (setLoc     :: Loc -> IO ())                    <- ask
    (movePlayer :: (Prop -> Prop) -> IO ())         <- ask
    (findProp   :: Loc -> PropId -> B (Maybe Prop)) <- ask

    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey . head $ tags p1
        id2 = maybe undefined id . view propKey . head $ tags p2
        f d i = tagging . set interaction . Just $ do
            pos <- fmap (maybe origin center) . sample $ findProp d i
            liftIO $ do
                setLoc d
                movePlayer $ teleport pos

    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

portalGen :: Some r => Eff r Prop
portalGen = do
    idkey <- Just . PropId <$> int
    return . tagging (propKey .~ idkey)
           . traced yellow
           $ rect origin 40 40

