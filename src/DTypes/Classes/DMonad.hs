{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DMonad
  ( DMonad (..)
  , Diag (..)
  , dap
  ) where

import DTypes.Classes.DApplicative
import DTypes.Classes.DFunctor
import DTypes.Compose
import DTypes.Trafo

newtype Diag f a
  = Diag
  { getDiag :: f a a
  }

class DApplicative d => DMonad (d :: (k -> *) -> *) where
  dreturn :: (forall (a :: k). f a) -> d f
  dreturn = dpure
  djoin :: d (Compose d f) -> d (Diag f)
  djoin x = dbind x getCompose
  dbind :: d f -> (forall a. f a -> d (g a)) -> d (Diag g)
  dbind x f = djoin (Compose . f <<$>> x)

data FBiCoyonedaLike f g a b
  = FBiCoyonedaLike (f a -> g a) (f b)

dap :: DMonad d => d (f ==>> g) -> d f -> d g
dap x y =
  joinFBiCoyonedaLike <<$>>
  dbind x (\t -> (FBiCoyonedaLike (unTrafoComp t) <<$>> y))
  where
    joinFBiCoyonedaLike :: Diag (FBiCoyonedaLike f g) a -> g a
    joinFBiCoyonedaLike (Diag (FBiCoyonedaLike f z)) = f z
