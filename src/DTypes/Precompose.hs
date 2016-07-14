{-# LANGUAGE CPP #-}

module DTypes.Precompose
  ( Precompose (..)
  ) where

import DTypes.Classes
import DTypes.Compose
import DTypes.Trafo

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
import Data.Traversable (Traversable (..))
#endif

newtype Precompose d f g
  = Precompose
  { getPrecompose :: d (Compose g f)
  }

instance DFunctor d => DFunctor (Precompose d f) where
  dfmap f = Precompose . (composeMap f <<$>>) . getPrecompose
    where composeMap f' = Compose . f' . getCompose

instance DApplicative d => DApplicative (Precompose d f) where
  dpure x = Precompose (dpure (Compose x))
  Precompose f <<*>> Precompose x =
    Precompose (dliftA2 composeAp f x)
    where
      composeAp (Compose g) (Compose y) = Compose (g $$ y)

instance DTraversable d => DTraversable (Precompose d f) where
  dsequenceA (Precompose x) = Precompose <$> dtraverse assoc x
    where
      assoc :: Functor f => Compose (Compose f g) h a -> Compose f (Compose g h) a
      assoc (Compose (Compose y)) = Compose (Compose <$> y)
