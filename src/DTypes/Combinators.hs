{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module DTypes.Combinators
  ( FApply (..)
  , FProd (..)
  , FSum (..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>))
#endif

import DTypes.Classes
import DTypes.Compose
import DTypes.Trafo

newtype FApply (x :: k) (f :: k -> *)
  = FApply
  { unFApply :: f x
  }

instance DFunctor (FApply x) where
  dfmap f (FApply x) = FApply (f x)

instance DApplicative (FApply x) where
  dpure x = FApply x
  FApply x <<*>> FApply y = FApply (x $$ y)

instance DTraversable (FApply x) where
  dsequenceA (FApply (Compose x)) = FApply <$> x

data FProd (d1 :: k -> *) (d2 :: k -> *) (f :: k)
  = FProd (d1 f) (d2 f)

instance (DFunctor d1, DFunctor d2) => DFunctor (FProd d1 d2) where
  dfmap f (FProd x y) = FProd (dfmap f x) (dfmap f y)

instance (DApplicative d1, DApplicative d2) => DApplicative (FProd d1 d2) where
  dpure x = FProd (dpure x) (dpure x)
  FProd x1 x2 <<*>> FProd y1 y2 = FProd (x1 <<*>> y1) (x2 <<*>> y2)

instance (DTraversable d1, DTraversable d2) => DTraversable (FProd d1 d2) where
  dsequenceA (FProd x y) = FProd <$> dsequenceA x <*> dsequenceA y
  dtraverse f (FProd x y) = FProd <$> dtraverse f x <*> dtraverse f y
  -- TODO: implement more?

data FSum (d1 :: k -> *) (d2 :: k -> *) (f :: k)
  = FSum1 (d1 f) | FSum2 (d2 f)

instance (DFunctor d1, DFunctor d2) => DFunctor (FSum d1 d2) where
  dfmap f (FSum1 x) = FSum1 (dfmap f x)
  dfmap f (FSum2 y) = FSum2 (dfmap f y)

instance (DTraversable d1, DTraversable d2) => DTraversable (FSum d1 d2) where
  dsequenceA (FSum1 x) = FSum1 <$> dsequenceA x
  dsequenceA (FSum2 y) = FSum2 <$> dsequenceA y
