module RPG.Logic.Input
    ( InputFilter (..)
    , inputFilterAddr
    , gameInput
    , menuInput
    ) where

import RPG.Core
import Game.Sequoia.Keyboard
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M
import qualified Data.Traversable as T

data InputFilter = GameFilter
                 | MenuFilter
                 | NoneFilter
                 deriving (Show, Eq, Ord, Enum)

{-# NOINLINE inputFilter #-}
{-# NOINLINE inputFilterAddr #-}
inputFilter  :: Signal InputFilter
(inputFilter, inputFilterAddr) = newMailbox "input filter" GameFilter

inputFilterer :: InputFilter -> Signal [Key]
inputFilterer f = do
    input <- inputFilter
    if f == input
       then keysDown
       else return []

gameInput :: Signal [Key]
gameInput = inputFilterer GameFilter

menuInput :: Signal [Key]
menuInput = inputFilterer MenuFilter

