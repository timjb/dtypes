{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DTraversable
  ( DTraversable (..)
  , ftraverse'
  , fsequenceA'
  , ftoList
  ) where

import DTypes.Classes.DFunctor
import DTypes.Compose
import DTypes.Trafo

import Data.Functor.Identity (Identity (..))
#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Const (..))
#else
import Control.Applicative (Applicative (..), (<$>), Const (..))
#endif

class DFunctor r => DTraversable (r :: (k -> *) -> *) where
  {-# MINIMAL ftraverse | fsequenceA #-}
  ftraverse :: Applicative g => (f ==> Compose g h) -> r f -> g (r h)
  ftraverse f = fsequenceA . ffmap f
  fsequenceA :: Applicative g => r (Compose g h) -> g (r h)
  fsequenceA = ftraverse id
  -- TODO: more functions

instance (Traversable f, DTraversable r) => DTraversable (Compose f r) where
  fsequenceA (Compose x) = Compose <$> traverse fsequenceA x 

ftraverse' :: (DTraversable r, Applicative g) => (f ==> g) -> r f -> g (r Identity)
ftraverse' f = ftraverse (Compose . fmap Identity . f)

fsequenceA' :: (DTraversable r, Applicative f) => r f -> f (r Identity)
fsequenceA' = fsequenceA . ffmap (Compose . fmap Identity)

ftoList
  :: DTraversable (rec :: (* -> *) -> *)
  => rec (Const a)
  -> [a]
ftoList = getConst . ftraverse' (Const . (:[]) . getConst) -- robot monkey!

{-
recFoldMap
  :: (Monoid m, DTraversable rec)
  => (forall a. f a -> m) -> rec f -> m
recFoldMap f = getConst . recTraverse (Const . f)

recFold
  :: (Monoid m, DTraversable rec)
  => rec (Const m) -> m
recFold = getConst . recSequenceA

recToList
  :: DTraversable rec
  => rec (Const a) -> [a]
recToList = recFoldMap (pure . getConst)
-}
