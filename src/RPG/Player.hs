{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RPG.Player
    ( newPlayer
    ) where

import RPG.Core
import RPG.Data.Gen.Player
import Game.Sequoia.Color
import Game.Sequoia.Keyboard (arrows)

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
        foldmp player $ \sq -> do
            dt     <- sample clock
            dpos   <- sample $ arrows keys
            walls  <- sample $ filter (_hasCollision . getTag) <$> scene
            floors <- sample $ filter (_isFloor      . getTag) <$> scene
            return $ move (scaleRel (300 * dt) dpos) sq

