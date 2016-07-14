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

class DFunctor (d :: (k -> *) -> *) where
  -- axioms:
  -- dfmap id = id
  -- dfmap (f . g) = fmap1 f . fmap1 g
  dfmap :: (f ==> g) -> d f -> d g

  -- | Replace all locations in the input with the same value.
  -- The default definition is @'fmap' . 'const'@, but this may be
  -- overridden with a more efficient version.
  (<<$) :: (forall a. g a) -> d f -> d g
  x <<$ d = dfmap (const x) d

-- synonym
(<<$>>) :: DFunctor d => (f ==> g) -> d f -> d g
(<<$>>) = dfmap

instance (Functor f, DFunctor d) => DFunctor (Compose f d) where
  dfmap t (Compose x) = Compose (dfmap t <$> x)