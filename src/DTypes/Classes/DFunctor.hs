{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DFunctor
  ( DFunctor (..)
  , (<<$>>)
  ) where

import DTypes.Compose
import DTypes.Trafo

infixl 4 <<$>>, <<$

class DFunctor (r :: (k -> *) -> *) where
  -- axioms:
  -- ffmap id = id
  -- ffmap (f . g) = fmap1 f . fmap1 g
  ffmap :: (f ==> g) -> r f -> r g

  -- | Replace all locations in the input with the same value.
  -- The default definition is @'fmap' . 'const'@, but this may be
  -- overridden with a more efficient version.
  (<<$) :: (forall a. g a) -> r f -> r g
  x <<$ r = ffmap (const x) r

-- synonym
(<<$>>) :: DFunctor r => (f ==> g) -> r f -> r g
(<<$>>) = ffmap

instance (Functor f, DFunctor r) => DFunctor (Compose f r) where
  ffmap t (Compose x) = Compose (ffmap t <$> x)