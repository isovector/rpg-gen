module RPG.Data.City
    ( City (..)
    , Size (..)
    ) where

data Size = Tiny | Small | Medium | Large | Huge

data City = City
    { hasInn  :: Bool
    -- TODO(sandy): specialize later
    , hasShop :: Bool
    , size :: Size
    }

