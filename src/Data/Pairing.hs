-- from https://github.com/dalaing/cofun/blob/2254b7da8a340b658c418823911f28be637dbc84/code/cofun-pairing/src/Util/Pairing.hs

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Pairing
    ( Pairing(..)
    , pairEffect
    ) where

import Control.Comonad (Comonad, extract)
import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Comonad.Trans.Cofree (CofreeT ())
import Control.Monad.Free (Free (..))
import Control.Monad.Trans.Free (runFree)
import qualified Control.Monad.Trans.Free as TF
import Data.Functor.Identity (Identity (..))

class Pairing f g | f -> g, g -> f where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
    pair f (Identity a) (Identity b) = f a b

instance Pairing f g => Pairing (Cofree f) (Free g) where
    pair p (a :< _ ) (Pure x)  = p a x
    pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance Pairing ((->) a) ((,) a) where
    pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
    pair p f g = p (snd f) (g (fst f))

pairEffect :: (Pairing f g, Comonad w, Functor f, Functor g)
           => (a -> b -> r)
           -> CofreeT f w a
           -> TF.Free g b
           -> r
pairEffect p s c =
    case runFree c of
      TF.Pure x -> p (extract s) x
      TF.Free gs -> pair (pairEffect p) (unwrap s) gs

