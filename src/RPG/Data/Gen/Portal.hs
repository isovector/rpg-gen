module RPG.Data.Gen.Portal
    ( portal
    ) where

import Game.Sequoia.Color (yellow)
import RPG.Core
import RPG.Scene

portal :: Loc -> Loc -> Some (Prop, Prop)
portal dst1 dst2 = do
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view propKey $ getTag p1
        id2 = maybe undefined id . view propKey $ getTag p2
        -- f d i = tags (interaction .~ (Just $ Teleport d i))
        f d i = id

    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

portalGen :: Some Prop
portalGen = do
    idkey <- Just <$> int
    return . tags (propKey .~ idkey)
           . traced yellow
           $ rect origin 40 40

