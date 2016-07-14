{-# LANGUAGE TypeFamilies #-}

module DTypes.Classes.HasDType
  ( HasDType (..)
  ) where

import Data.Functor.Identity (Identity (..))

class HasDType t where
  type DType t :: (* -> *) -> *
  diso :: t -> DType t Identity
  dosi :: DType t Identity -> t
