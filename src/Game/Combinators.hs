module Game.Combinators
    ( eraser
    ) where

import Preface

import Control.Arrow (first)
import Data.List (nub)
import Game.Sequoia
import Game.Sequoia.Utils

eraser :: Signal [Prop] -> (Prop -> Bool) -> Signal Prop -> Signal [Prop]
eraser sps f sdozer =
    let remSig = foldp doze [] ((,) <$> sps <*> sdozer)
     in filterOutIdx <$> remSig <*> sps
  where
    doze :: ([Prop], Prop) -> [Int] -> [Int]
    doze (ps', dozer) rem =
        let ps    = filterOutIdx rem ps'
            hit   = overlapping ps dozer
            dozed = nub $ filter f hit
         in if null dozed
               then rem
               else rem ++ idxsOf ps' dozed

    filterOutIdx :: [Int] -> [a] -> [a]
    filterOutIdx idx = map snd . filter (flip notElem idx . fst) . zip [0..]

    idxsOf :: Eq a => [a] -> [a] -> [Int]
    idxsOf all which = map fst . filter (flip elem which . snd) $ zip [0..] all

