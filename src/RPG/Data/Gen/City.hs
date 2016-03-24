module RPG.Data.Gen.City
    ( City (..)
    , cityGen
    ) where

import RPG.Core
import Game.Sequoia.Color (rgb, red)

data City = City
    { surroundings :: [Prop]
    }

cityGen :: Prop -> Some City
cityGen p = do
    width  <- uniformIn (100, 200)
    height <- uniformIn (100, 200)
    surroundings <- surroundingsGen width height
    pdx <- (/ 1.5) <$> uniformIn (-width, width)
    pdy <- (/ 1.5) <$> uniformIn (-height, height)
    ecotone <- ecotoneGen (mkPos 100 (-80)) rockGen (mkPos 150 (-80)) treeGen
    return . City $ move (mkRel pdx pdy) p : (surroundings ++ ecotone)

surroundingsGen :: Double -> Double -> Some [Prop]
surroundingsGen width' height' = do
    let width = width' * 2
        height = height' * 2
    obstacleGen <- uniformly [treeGen, rockGen]
    numTrees <- uniformIn (50, 150)
    trees <- listOf numTrees obstacleGen

    forM trees $ \tree -> do
        (xoffset', yoffset') <- uniformly [ (-1,  0)
                                          , ( 1,  0)
                                          , ( 0, -1)
                                          , ( 0,  1)
                                          ]
        let xoffset = xoffset' * width'
            yoffset = yoffset' * height'
        xspread <-
            if xoffset' == 0
               then uniformIn (-width / 2, width / 2)
               else uniformIn (-50, 50)
        yspread <-
            if yoffset' == 0
               then (subtract $ height/2) <$> uniformIn (0, height)
               else uniformIn (-50, 50)
        return $ move (mkRel (xoffset + xspread) (yoffset + yspread)) tree

transitionGen :: Some a -> Some a -> Double -> Some a
transitionGen a b weight =
    join $ weighted [ (a, 1 - weight)
                    , (b, weight)
                    ]

ecotoneGen :: Pos -> Some Prop -> Pos -> Some Prop -> Some [Prop]
ecotoneGen src srcGen dst dstGen = do
    let start   = posDif src origin
        dist    = distance src dst
        dir     = scaleRel 40 $ posDif dst src
        size    = mag dir / 20
        numObs  = round $ size
        samples = map ((/size) . fromIntegral) [0..numObs]
        obsGen  = transitionGen srcGen dstGen
    forM samples $ \sample -> do
        obs <- obsGen sample
        return $ move (start + scaleRel sample dir) obs

rockGen :: Some Prop
rockGen = do
    color <- rgb <$> uniformIn (0.3, 0.6)
                 <*> uniformIn (0, 0.4)
                 <*> uniformIn (0.3, 1)
    radius <- uniformIn (5, 15)
    return . tags (hasCollision .~ True)
           . filled color
           $ circle origin radius

treeGen :: Some Prop
treeGen = do
    color <- rgb <$> uniformIn (0.2, 0.4)
                 <*> uniformIn (0.5, 1.0)
                 <*> uniformIn (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . tags (hasCollision .~ True)
           . filled color
           $ polygon origin [ mkRel 0 (-height / 2)
                            , mkRel  (width / 2) (height / 2)
                            , mkRel (-width / 2) (height / 2)
                            ]

