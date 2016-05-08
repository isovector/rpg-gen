{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RPG.Player
    ( newPlayer
    ) where

import Control.FRPNow.EvStream
import Control.Monad (forM_)
import Data.Maybe (isJust, mapMaybe)
import Game.Sequoia.Color
import Game.Sequoia.Keyboard (arrows, keyPress)
import RPG.Core
import RPG.Data.Gen.Player
import RPG.Menu

withMailbox :: Prop
            -> (Prop -> N Prop)
            -> N ( B Prop
                 , (Prop -> Prop) -> IO ()
                 )
withMailbox p f = do
    r@(_, b) <- foldmp p f
    sync . b $ tags (box .~ Just b)
    return r


newPlayer :: ( Has (B Time)           r
             , Has (B [Key])          r
             , Has (B [Prop])         r
             , Has (B (Maybe [Prop])) r
             )
          => Eff r (N ( B Prop
                      , (Prop -> Prop) -> IO ()
                      ))
newPlayer = do
    (clock  :: B Time)   <- ask
    (keys   :: B [Key])  <- ask
    (scene  :: B [Prop]) <- ask
    (menu   :: B (Maybe [Prop])) <- ask
    let menuUp = isJust <$> menu
    return $ traceChanges "up" menuUp

    return $ do
        player <- sync $ pick playerGen
        up <- sample menuUp
        r@(sig, addr) <- withMailbox player $ \p -> do
            dt   <- sample clock
            dpos <- if up
                       then fmap (scaleRel $ dt * 300) . sample $ arrows keys
                       else return $ mkRel 0 0
            ps   <- sample scene
            let walls  = filter (_hasCollision . getTag) ps
                floors = filter (_isFloor      . getTag) ps
            return $ tryMove walls floors p dpos

        onEvent (keyPress keys SpaceKey) . const $ do
            p  <- sample sig
            ps <- sample scene
            up <- sample menuUp
            when (not up) .
                sync $ forM_ (interactions ps p) id

        return r


interactions :: [Prop]
             -> Prop
             -> [IO ()]
interactions ps p =
    mapMaybe (view interaction)
        . map getTag
        $ overlapping ps p

