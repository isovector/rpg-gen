module RPG.Data.Gen.Portal
    ( portal
    ) where

import Game.Sequoia.Color (yellow)
import Game.Sequoia.Utils
import RPG.Core
import RPG.Scene

portal :: (Loc -> IO ())
       -> Loc
       -> Loc
       -> Some (Prop, Prop)
portal setLoc dst1 dst2 = do
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey $ getTag p1
        id2 = maybe undefined id . view propKey $ getTag p2
        f d i = tags $ interaction .~ Just (setLoc $ showTrace d)

    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

portalGen :: Some Prop
portalGen = do
    idkey <- Just <$> int
    return . tags (propKey .~ idkey)
           . traced yellow
           $ rect origin 40 40

