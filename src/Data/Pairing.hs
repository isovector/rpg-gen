{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Pairing
    ( Zap(..)
    , pairEffect
    ) where

import Control.Comonad (Comonad, extract)
import Control.Comonad.Cofree (Cofree (..), unwrap)
import Control.Comonad.Trans.Cofree (CofreeT ())
import Control.Monad.Free (Free (..))

class Zap f g | f -> g, g -> f where
    zap :: (a -> b -> r) -> f a -> g b -> r

instance {-# OVERLAPPABLE #-} Zap f g => Zap g f where
    zap f a b = zap (flip f) b a

instance Zap f g => Zap (Free f) (Cofree g) where
    zap p (Pure a ) (b :< _ ) = p a b
    zap p (Free as) (_ :< bs) = zap (zap p) as bs

instance Zap ((,) a) ((->) a) where
    zap f (x, a) xtob = f a (xtob x)

pairEffect :: ( Zap f g
              , Comonad w
              , Functor g
              )
           => (a -> b -> c)
           -> Free f a
           -> CofreeT g w b
           -> c
pairEffect f (Pure a ) b =                 f  a  $ extract b
pairEffect f (Free as) b = zap (pairEffect f) as $ unwrap  b

