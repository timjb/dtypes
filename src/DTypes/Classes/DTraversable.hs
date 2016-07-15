{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DTraversable
  ( DTraversable (..)
  , dtraverse'
  , dsequenceA'
  , dtoList
  ) where

import DTypes.Classes.DFunctor
import DTypes.Compose
import DTypes.Trafo

import Data.Functor.Identity (Identity (..))
#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Const (..))
#else
import Control.Applicative (Applicative (..), (<$>), Const (..))
import Data.Traversable (Traversable (..))
#endif

class DFunctor d => DTraversable (d :: (k -> *) -> *) where
  {-# MINIMAL dtraverse | dsequenceA #-}
  dtraverse :: Applicative g => (f ==> Compose g h) -> d f -> g (d h)
  dtraverse f = dsequenceA . dfmap f
  dsequenceA :: Applicative g => d (Compose g h) -> g (d h)
  dsequenceA = dtraverse id
  -- TODO: more functions

instance (Traversable f, DTraversable d) => DTraversable (Compose f d) where
  dsequenceA (Compose x) = Compose <$> traverse dsequenceA x 

dtraverse' :: (DTraversable d, Applicative g) => (f ==> g) -> d f -> g (d Identity)
dtraverse' f = dtraverse (Compose . fmap Identity . f)

dsequenceA' :: (DTraversable d, Applicative f) => d f -> f (d Identity)
dsequenceA' = dsequenceA . dfmap (Compose . fmap Identity)

dtoList
  :: DTraversable (d :: (* -> *) -> *)
  => d (Const a)
  -> [a]
dtoList = getConst . dtraverse' (Const . (:[]) . getConst) -- robot monkey!

{-
dFoldMap
  :: (Monoid m, DTraversable d)
  => (forall a. f a -> m) -> d f -> m
dFoldMap f = getConst . dTraverse (Const . f)

dFold
  :: (Monoid m, DTraversable d)
  => d (Const m) -> m
dFold = getConst . dSequenceA

dToList
  :: DTraversable d
  => d (Const a) -> [a]
dToList = dFoldMap (pure . getConst)
-}
