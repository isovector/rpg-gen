module Game.Gen
    ( pick
    , picking
    , portal
    , teleportTo
    ) where

import Preface

import Data.Some
import Game.Scene
import Game.Sequoia.Color
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

{-# NOINLINE picking #-}
picking :: Some a -> (a -> b) -> b
picking a f = f . unsafePerformIO $ pick a

portal :: LocKey -> LocKey -> Some (Prop, Prop)
portal dst1 dst2 = do
    p1 <- portalGen
    p2 <- portalGen
    let id1 = maybe undefined id . view ident $ getTag p1
        id2 = maybe undefined id . view ident $ getTag p2
        f d i = tags (interaction .~ (Just $ Teleport d i))
    return ( f dst2 id2 p1
           , f dst1 id1 p2
           )

teleportTo :: Map LocKey [Prop] -> LocKey -> Int -> Prop -> Prop
teleportTo ls l i p = maybe p id $ do
    loc <- M.lookup l ls
    dst <- findProp loc i
    return . mailing changeScene l $ teleport (center dst) p

portalGen :: Some Prop
portalGen = do
    idkey <- Just <$> int
    return . tags (ident .~ idkey) . traced yellow $ rect origin 40 40

