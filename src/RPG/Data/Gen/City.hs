{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPG.Data.Gen.City
    ( City (..)
    , cityGen
    , houseGen
    , locGen
    ) where

import Control.Eff (run)
import RPG.Core
import RPG.Scene
import Control.Monad (forM, join)
import RPG.Data.Gen.Person
import RPG.Data.Gen.Portal
import RPG.Data.Gen.Utils
import Game.Sequoia.Color
import qualified Data.Map as M
import qualified System.Random.MWC as MWC

data Size = Tiny | Small | Medium | Large | Huge
    deriving (Eq, Show, Ord, Enum)

data City = City
    { hasInn  :: Bool
    -- TODO(sandy): specialize later
    , hasShop :: Bool
    , size    :: Size
    }

locGen :: Some r
       => Eff r Loc
locGen = Loc <$> uniform minBound maxBound

whGen :: (Some r, Num a, MWC.Variate a) => Size -> Eff r (a, a)
whGen s = do
    let wh = go s
    w <- uniformIn wh
    h <- uniformIn wh
    return (w, h)
  where
    go Tiny   = (250, 350)
    go Small  = (350, 500)
    go Medium = (500, 700)
    go Large  = (700, 900)
    go Huge   = (900, 1200)

cityGen2 :: ( Some r
            , Has (Loc -> B [Prop] -> IO ()) r
            , Has (Loc -> IO ()) r
            , Has ((Prop -> Prop) -> IO ()) r
            , Has (Loc -> PropId -> B (Maybe Prop)) r
            , Has (B Prop -> Time -> IO ()) r
            )
         => City
         -> Loc
         -> Eff r [Prop]
cityGen2 c loc = do
    let s         = size c
        numHouses = fromEnum s * 2 + 3
    numRows <- do
        delta  <- uniform 1 $ numHouses `div` 2
        return $ numHouses - delta
    (w, h) <- whGen s
    houses <- listOf numHouses $ houseGen loc
    fmap join . forM houses $ \house -> do
        xspread <- (/2) <$> uniformIn (-w, w)
        row <- (150 *) <$> uniformIn (0, numRows)
        return $ map (move . mkRel xspread $ fromIntegral row) house

cityGen :: ( Some r
           , Has (Loc -> B [Prop] -> IO ()) r
           , Has (Loc -> IO ()) r
           , Has ((Prop -> Prop) -> IO ()) r
           , Has (Loc -> PropId -> B (Maybe Prop)) r
           , Has (B Prop -> Time -> IO ()) r
           )
        => Loc
        -> Eff r (B [Prop])
cityGen loc =
    fmap return $ cityGen2 (City True True Medium) loc
    -- do
    -- width  <- uniformIn (100, 200)
    -- height <- uniformIn (100, 200)
    -- house <- houseGen loc
    -- surroundings <- surroundingsGen width height
    -- actors <- listOf 5 actorGen
    -- hdx <- (/ 1.5) <$> uniformIn (-width, width)
    -- hdy <- (/ 1.5) <$> uniformIn (-height, height)
    -- ecotone <- ecotoneGen (mkPos 100 (-80)) rockGen
    --                       (mkPos 150 (-80)) treeGen
    -- return . sequence
    --        $ fmap (return . Just) surroundings
    --        -- ++ map (move (mkRel hdx hdy)) house
    --        -- ++ fmap (return . Just) ecotone
    --        ++ actors

surroundingsGen :: Some r => Double -> Double -> Eff r [Prop]
surroundingsGen width' height' = do
    let width = width' * 2
        height = height' * 2
    obstacleGen <- uniformly [treeGen, rockGen]
    numTrees <- uniformIn (50, 150)
    -- TODO(sandy): bug here
    trees <- listOf numTrees obstacleGen

    forM trees $ \tree -> do
        (xoffset', yoffset') <- uniformly $ [ (-1,  0)
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

transitionGen :: Some r => Eff r a -> Eff r a -> Double -> Eff r a
transitionGen a b weight =
    join $ weighted [ (a, 1 - weight)
                    , (b, weight)
                    ]

interiorGen :: ( Some r
               , Has (B Prop -> Time -> IO ()) r
               , Has (Loc -> PropId -> B (Maybe Prop)) r
               )
            => Double
            -> Double
            -> Loc
            -> Prop
            -> Eff r [Prop]
interiorGen width height loc portal = do
    let depth = 40
        floor = rect origin (width + depth) (height + depth)
        sideWall = rect origin depth $ height + 2 * depth
        topWall = rect origin width depth
        botWall = rect origin ((width - depth) / 2) depth
        yshift = (height + depth) / 2
        xshift = (width + depth) / 2
        tag = tags $ hasCollision .~ True
    who <- personGen >>= personBuild loc

    return $
           [ tags (isFloor .~ True) $ filled red floor
           , tag . move (mkRel 0 (-yshift)) $ filled grey topWall
           , tag . move (mkRel (-xshift) 0) $ filled grey sideWall
           , tag . move (mkRel xshift 0) $ filled grey sideWall
           , tag . move (mkRel (xshift / 2) yshift) $ filled grey botWall
           , tag . move (mkRel ((-xshift) / 2) yshift) $ filled grey botWall
           , move (mkRel 0 yshift) portal
           ]
           ++
           who

houseGen :: ( Some r
            , Has (Loc -> B [Prop] -> IO ()) r
            , Has (Loc -> IO ()) r
            , Has ((Prop -> Prop) -> IO ()) r
            , Has (Loc -> PropId -> B (Maybe Prop)) r
            , Has (B Prop -> Time -> IO ()) r
            )
         => Loc
         -> Eff r [Prop]
houseGen loc = do
    (addScene :: Loc -> B [Prop] -> IO ()) <- ask
    intLoc <- locGen
    (p1, p2) <- portal loc intLoc
    interior <- interiorGen 200 200 intLoc p2

    liftIO . addScene intLoc $ pure interior

    slatColor <- colorFuzz 0.2 grey
    roofColor <- colorFuzz 0.3 yellow
    -- slope  <- uniform 20 50
    -- height <- uniform 50 100
    halfWidth <- uniform 30 40
    let height = halfWidth
        slope = halfWidth
        depth = halfWidth

    let facade = polygon origin [ mkRel halfWidth 0
                                , mkRel (-halfWidth) 0
                                , mkRel (-halfWidth) (-height)
                                , mkRel 0 . negate $ height + slope
                                , mkRel halfWidth (-height)
                                ]
        roof1 = polygon origin [ mkRel 0 0
                               , mkRel (-halfWidth) slope
                               , mkRel (-halfWidth) (-depth)
                               , mkRel 0 . negate $ slope + depth
                               ]
        roof2 = polygon origin [ mkRel 0 . negate $ slope + depth
                               , mkRel halfWidth (-depth)
                               , mkRel halfWidth slope
                               , mkRel 0 0
                               ]
        brown = rgb 0.3 0.1 0
        roofLining = defaultLine { lineColor = brown
                                 , lineWidth = 5
                                 }
        door = rect origin 20 36
        tag = tags (hasCollision .~ True)
    return [ move (mkRel 0 5) p1
           , tag $ filled slatColor facade
           , tag . move (mkRel 0 . negate $ height + slope)
               $ styled roofColor roofLining roof1
           , tag . move (mkRel 0 . negate $ height + slope)
               $ styled roofColor roofLining roof2
           , tag . move (mkRel 0 (-18)) $ filled brown door
           ]


ecotoneGen :: Some r
           => Pos
           -> Eff r Prop
           -> Pos
           -> Eff r Prop
           -> Eff r [Prop]
ecotoneGen src srcGen dst dstGen = do
    let start   = posDif src origin
        dist    = distance src dst
        dir     = scaleRel 40 $ posDif dst src
        size    = mag dir / 20
        numObs :: Int
        numObs  = round size
        samples = map ((/size) . fromIntegral) [0..numObs]
    forM samples $ \sample -> do
        obs <- transitionGen srcGen dstGen sample
        return $ move (start + scaleRel sample dir) obs

rockGen :: Some r => Eff r Prop
rockGen = do
    color <- colorGen (0.3, 0.6) (0, 0.4) (0.3, 1)
    radius <- uniformIn (5, 15)
    return . tags (hasCollision .~ True)
           . filled color
           $ circle origin radius

treeGen :: Some r => Eff r Prop
treeGen = do
    color <- colorGen (0.2, 0.4) (0.5, 1.0) (0, 0.3)
    width <- uniformIn (10, 30)
    heightMod <- uniformIn (1.5, 3)
    let height = width * heightMod
    return . tags (hasCollision .~ True)
           . filled color
           $ polygon origin [ mkRel 0 (-height / 2)
                            , mkRel  (width / 2) (height / 2)
                            , mkRel (-width / 2) (height / 2)
                            ]

