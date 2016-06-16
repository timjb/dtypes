{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module FTypes.Combinators
  ( FApply (..)
  , FProd (..)
  , FSum (..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>))
#endif

import FTypes.Classes
import FTypes.Compose
import FTypes.Trafo

newtype FApply (x :: k) (f :: k -> *) = FApply { unFApply :: f x }

instance FFunctor (FApply x) where
  ffmap f (FApply x) = FApply (f x)

instance FApplicative (FApply x) where
  fpure x = FApply x
  FApply x <<*>> FApply y = FApply (x $$ y)

instance FTraversable (FApply x) where
  fsequenceA (FApply (Compose x)) = FApply <$> x

data FProd (r :: k -> *) (s :: k -> *) (f :: k) = FProd (r f) (s f)

instance (FFunctor r, FFunctor s) => FFunctor (FProd r s) where
  ffmap f (FProd x y) = FProd (ffmap f x) (ffmap f y)

instance (FApplicative r, FApplicative s) => FApplicative (FProd r s) where
  fpure x = FProd (fpure x) (fpure x)
  FProd x1 x2 <<*>> FProd y1 y2 = FProd (x1 <<*>> y1) (x2 <<*>> y2)

instance (FTraversable r, FTraversable s) => FTraversable (FProd r s) where
  fsequenceA (FProd x y) = FProd <$> fsequenceA x <*> fsequenceA y
  ftraverse f (FProd x y) = FProd <$> ftraverse f x <*> ftraverse f y
  -- TODO: implement more?

data FSum (r :: k -> *) (s :: k -> *) (f :: k) = FSum1 (r f) | FSum2 (s f)

instance (FFunctor r, FFunctor s) => FFunctor (FSum r s) where
  ffmap f (FSum1 x) = FSum1 (ffmap f x)
  ffmap f (FSum2 y) = FSum2 (ffmap f y)

instance (FTraversable r, FTraversable s) => FTraversable (FSum r s) where
  fsequenceA (FSum1 x) = FSum1 <$> fsequenceA x
  fsequenceA (FSum2 y) = FSum2 <$> fsequenceA y
