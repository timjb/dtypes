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

newtype Precompose r f g
  = Precompose
  { getPrecompose :: r (Compose g f)
  }

instance DFunctor r => DFunctor (Precompose r f) where
  ffmap f = Precompose . (composeMap f <<$>>) . getPrecompose
    where composeMap f' = Compose . f' . getCompose

instance DApplicative r => DApplicative (Precompose r f) where
  fpure x = Precompose (fpure (Compose x))
  Precompose f <<*>> Precompose x =
    Precompose (fliftA2 composeAp f x)
    where
      composeAp (Compose g) (Compose y) = Compose (g $$ y)

instance DTraversable r => DTraversable (Precompose r f) where
  fsequenceA (Precompose x) = Precompose <$> ftraverse assoc x
    where
      assoc :: Functor f => Compose (Compose f g) h a -> Compose f (Compose g h) a
      assoc (Compose (Compose y)) = Compose (Compose <$> y)
