{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RPG.Player
    ( newPlayer
    ) where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import RPG.Core
import RPG.Data.Gen.Player
import Game.Sequoia.Color
import Game.Sequoia.Keyboard (arrows)

withMailbox :: Prop
            -> (Prop -> Now Prop)
            -> N ( B Prop
                 , (Prop -> Prop) -> IO ()
                 )
withMailbox p f = do
    r@(_, b) <- foldmp p f
    sync . b $ tags (box .~ Just b)
    return r


newPlayer :: ( Has (Behavior Time)   r
             , Has (Behavior [Key])  r
             , Has (Behavior [Prop]) r
             )
          => Eff r (Now ( Behavior Prop
                        , Address (Prop -> Prop)
                        ))
newPlayer = do
    (clock :: Behavior Time)   <- ask
    (keys  :: Behavior [Key])  <- ask
    (scene :: Behavior [Prop]) <- ask

    return $ do
        player <- sync $ pick playerGen
        withMailbox player $ \p -> do
            dt     <- sample clock
            dpos   <- fmap (scaleRel $ dt * 300) . sample $ arrows keys
            ps     <- sample scene
            let walls     = filter (_hasCollision . getTag) ps
                floors    = filter (_isFloor      . getTag) ps
                addr      = maybe (error "no box!") id . view box $ getTag p

            forM_ (interactions ps p) ($ addr)
            return $ tryMove walls floors p dpos


interactions :: [Prop]
             -> Prop
             -> [((Prop -> Prop) -> IO ()) -> Now ()]
interactions ps p =
    mapMaybe (view interaction)
           . map getTag
           $ overlapping ps p

