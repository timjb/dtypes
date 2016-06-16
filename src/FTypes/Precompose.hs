{-# LANGUAGE CPP #-}

module FTypes.Precompose
  ( Precompose (..)
  ) where

import FTypes.Classes
import FTypes.Compose
import FTypes.Trafo

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
import Data.Traversable (Traversable (..))
#endif

newtype Precompose r f g
  = Precompose { getPrecompose :: r (Compose g f) }

instance FFunctor r => FFunctor (Precompose r f) where
  ffmap f = Precompose . (composeMap f <<$>>) . getPrecompose
    where composeMap f' = Compose . f' . getCompose

instance FApplicative r => FApplicative (Precompose r f) where
  fpure x = Precompose (fpure (Compose x))
  Precompose f <<*>> Precompose x =
    Precompose (fliftA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (g $$ y)

instance FTraversable r => FTraversable (Precompose r f) where
  fsequenceA (Precompose x) = Precompose <$> ftraverse assoc x
    where
      assoc :: Functor f => Compose (Compose f g) h a -> Compose f (Compose g h) a
      assoc (Compose (Compose y)) = Compose (fmap Compose y) -- TODO: optimize
