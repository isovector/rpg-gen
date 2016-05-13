{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module RPG.Data.Gen.Portal
    ( portal
    ) where

import Game.Sequoia.Color (yellow)
import Game.Sequoia.Utils
import RPG.Core
import RPG.Scene

portal :: ( Some r
          , Has (Loc -> IO ()) r
          )
       => Loc
       -> Loc
       -> Eff r (Prop, Prop)
portal dst1 dst2 = do
    setLoc <- ask
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey $ getTag p1
        id2 = maybe undefined id . view propKey $ getTag p2
        f d i = tags $ interaction .~ Just (setLoc $ showTrace d)

    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

portalGen :: Some r => Eff r Prop
portalGen = do
    idkey <- Just <$> int
    return . tags (propKey .~ idkey)
           . traced yellow
           $ rect origin 40 40

