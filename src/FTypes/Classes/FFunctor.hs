{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Classes.FFunctor
  ( FFunctor (..)
  , (<<$>>)
  ) where

import FTypes.Trafo

infixl 4 <<$>>, <<$

class FFunctor (r :: (k -> *) -> *) where
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
(<<$>>) :: FFunctor r => (f ==> g) -> r f -> r g
(<<$>>) = ffmap
