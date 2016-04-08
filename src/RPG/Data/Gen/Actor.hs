{-# LANGUAGE Rank2Types #-}
module RPG.Data.Gen.Actor
    ( actorGen
    ) where

import Control.Monad.IO.Class (liftIO)
import RPG.Core
import RPG.Data.Gen.Utils
import RPG.Logic.Scene
import RPG.Logic.Combat (makeActor)
import Game.Sequoia.Color

actorGen :: Some (Signal (Maybe Prop))
actorGen = do
    color <- colorGen (0, 1) (0, 1) (0, 1)
    maxHp <- uniform 10 100
    maxMp <- uniform 10 100
    x <- uniform (-100) 100
    y <- uniform (-100) 100
    let (prop, addr) =
            foldmp (Just . filled color $ rect (mkPos x y) 20 20) $ \p -> do
                let hp' = maybe 1 id $ do
                            prop <- p
                            actor <- view propActor $ getTag prop
                            return $ view hp actor
                return $ if hp' > 0
                   then p
                   else Nothing
    liftIO . sampleAt 0
           . makeActor addr
           $ Actor maxHp maxMp 1 undefined
    return prop

