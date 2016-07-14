{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module DTypes.Classes.HasDualDType
  ( HasDualDType (..)
  , Elim (..)
  ) where

import DTypes.Classes.HasDType

newtype Elim f x a
  = Elim
  { getElim :: f a -> x
  }

class HasDType t => HasDualDType t where
  type DualDType t :: (* -> *) -> *
  dpair
    :: (forall a. f a -> g a -> x) 
    -> DualDType t f
    -> DType t g
    -> x
