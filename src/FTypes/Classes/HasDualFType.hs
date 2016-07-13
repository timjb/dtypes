{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module FTypes.Classes.HasDualFType
  ( HasDualFType (..)
  , Elim (..)
  ) where

import FTypes.Classes.HasFType

newtype Elim f x a
  = Elim
  { getElim :: f a -> x
  }

class HasFType t => HasDualFType t where
  type DualFType t :: (* -> *) -> *
  fpair
    :: (forall a. f a -> g a -> x) 
    -> DualFType t f
    -> FType t g
    -> x
  --fpair :: DualFType t (Elim f x) -> FType t f -> x
