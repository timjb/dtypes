{-# LANGUAGE TypeFamilies #-}

module DTypes.Classes.HasDType
  ( HasDType (..)
  ) where

import Data.Functor.Identity (Identity (..))

class HasDType t where
  type DType t :: (* -> *) -> *
  fiso :: t -> DType t Identity
  fosi :: DType t Identity -> t
