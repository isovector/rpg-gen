{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RPG.Player
    ( newPlayer
    ) where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Control.FRPNow.EvStream
import RPG.Core
import RPG.Data.Gen.Player
import Game.Sequoia.Color
import Game.Sequoia.Keyboard (arrows, keyPress)

withMailbox :: Prop
            -> (Prop -> N Prop)
            -> N ( B Prop
                 , (Prop -> Prop) -> IO ()
                 )
withMailbox p f = do
    r@(_, b) <- foldmp p f
    sync . b $ tags (box .~ Just b)
    return r


newPlayer :: ( Has (B Time)   r
             , Has (B [Key])  r
             , Has (B [Prop]) r
             )
          => Eff r (N ( B Prop
                      , Address (Prop -> Prop)
                      ))
newPlayer = do
    (clock :: B Time)   <- ask
    (keys  :: B [Key])  <- ask
    (scene :: B [Prop]) <- ask

    return $ do
        player <- sync $ pick playerGen
        r@(sig, addr) <- withMailbox player $ \p -> do
            dt   <- sample clock
            dpos <- fmap (scaleRel $ dt * 300) . sample $ arrows keys
            ps   <- sample scene
            let walls  = filter (_hasCollision . getTag) ps
                floors = filter (_isFloor      . getTag) ps
            return $ tryMove walls floors p dpos

        onEvent (keyPress keys SpaceKey) . const $ do
            p  <- sample sig
            ps <- sample scene
            sync $ forM_ (interactions ps p) id

        return r


interactions :: [Prop]
             -> Prop
             -> [IO ()]
interactions ps p =
    mapMaybe (view interaction)
        . map getTag
        $ overlapping ps p

