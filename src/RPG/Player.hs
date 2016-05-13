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
    (scene  :: B [Prop]) <- ask
    keys <- do
        (keyboard :: B [Key])          <- ask
        (menu     :: B (Maybe [Prop])) <- ask
        return $ ifThenElse
              <$> fmap isJust menu
              <*> return []
              <*> keyboard

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
            forM_ (interactions ps p) id

        return r


interactions :: [Prop]
             -> Prop
             -> [Now ()]
interactions ps p =
    mapMaybe (view interaction)
        . map getTag
        $ overlapping ps p


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y

