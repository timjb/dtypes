{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Classes.FTraversable
  ( FTraversable (..)
  , ftraverse'
  , fsequenceA'
  , ftoList
  ) where

import FTypes.Classes.FFunctor
import FTypes.Compose
import FTypes.Trafo

import Data.Functor.Identity (Identity (..))
#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Const (..))
#else
import Control.Applicative (Applicative (..), (<$>), Const (..))
#endif

class FFunctor r => FTraversable (r :: (k -> *) -> *) where
  {-# MINIMAL ftraverse | fsequenceA #-}
  ftraverse :: Applicative g => (f ==> Compose g h) -> r f -> g (r h)
  ftraverse f = fsequenceA . ffmap f
  fsequenceA :: Applicative g => r (Compose g h) -> g (r h)
  fsequenceA = ftraverse id
  -- TODO: more functions

ftraverse' :: (FTraversable r, Applicative g) => (f ==> g) -> r f -> g (r Identity)
ftraverse' f = ftraverse (Compose . fmap Identity . f)

fsequenceA' :: (FTraversable r, Applicative f) => r f -> f (r Identity)
fsequenceA' = fsequenceA . ffmap (Compose . fmap Identity)

ftoList
  :: FTraversable (rec :: (* -> *) -> *)
  => rec (Const a)
  -> [a]
ftoList = getConst . ftraverse' (Const . (:[]) . getConst) -- robot monkey!

{-
recFoldMap
  :: (Monoid m, FTraversable rec)
  => (forall a. f a -> m) -> rec f -> m
recFoldMap f = getConst . recTraverse (Const . f)

recFold
  :: (Monoid m, FTraversable rec)
  => rec (Const m) -> m
recFold = getConst . recSequenceA

recToList
  :: FTraversable rec
  => rec (Const a) -> [a]
recToList = recFoldMap (pure . getConst)
-}
