module Game.Combinators
    ( eraser
    ) where

import Preface
import Data.List (nub)
import Game.Sequoia
import Game.Sequoia.Utils

-- TODO(sandy): this stops working if `sps` is not static
-- TODO(sandy): maybe filter by index instead of by eq?
eraser :: Signal [Prop] -> (Prop -> Bool) -> Signal Prop -> Signal [Prop]
eraser sps f sdozer =
    let remSig = foldp doze [] ((,) <$> sps <*> sdozer)
     in (filter . flip notElem) <$> remSig <*> sps
  where
    doze (ps, dozer) rem =
        let hit   = showTrace $ overlapping ps dozer
            dozed = nub $ filter f hit
         in if null dozed
               then rem
               else nub $ rem ++ dozed

