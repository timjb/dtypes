{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Classes.FMonad
  ( FMonad (..)
  , Diag (..)
  , fap
  ) where

import FTypes.Classes.FApplicative
import FTypes.Classes.FFunctor
import FTypes.Compose
import FTypes.Trafo

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

data FBiCoyonedaLike f g a b
  = FBiCoyonedaLike (f a -> g a) (f b)

fap :: FMonad r => r (f ==>> g) -> r f -> r g
fap x y =
  joinFBiCoyonedaLike <<$>>
  fbind x (\t -> (FBiCoyonedaLike (unTrafoComp t) <<$>> y))
  where
    joinFBiCoyonedaLike :: Diag (FBiCoyonedaLike f g) a -> g a
    joinFBiCoyonedaLike (Diag (FBiCoyonedaLike f z)) = f z
