{-# LANGUAGE TypeFamilies #-}

module FTypes.Classes.HasFType
  ( HasFType (..)
  ) where

import Data.Functor.Identity (Identity (..))

class HasFType t where
  type FType t :: (* -> *) -> *
  fiso :: t -> FType t Identity
  fosi :: FType t Identity -> t
