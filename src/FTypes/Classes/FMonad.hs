{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}

module FTypes.Classes.FMonad
  ( FMonad (..)
  , Diag (..)
  ) where

import FTypes.Classes.FApplicative
import FTypes.Classes.FFunctor
import FTypes.Compose

newtype Diag f a
  = Diag
  { getDiag :: f a a
  }

class FApplicative r => FMonad (r :: (k -> *) -> *) where
  freturn :: (forall (a :: k). f a) -> r f
  freturn = fpure
  fjoin :: r (Compose r f) -> r (Diag f)
  fjoin x = fbind x getCompose
  fbind :: r f -> (forall a. f a -> r (g a)) -> r (Diag g)
  fbind x f = fjoin (Compose . f <<$>> x)
