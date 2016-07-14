{-# LANGUAGE CPP #-}

module DTypes.Postcompose
  ( Postcompose (..)
  ) where

import DTypes.Classes
import DTypes.Compose
import DTypes.Trafo

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative (Applicative (..), (<$>), liftA2)
import Data.Traversable (Traversable (..))
#endif

newtype Postcompose r f g
  = Postcompose
  { getPostcompose :: r (Compose f g)
  }

instance (DFunctor rec, Functor f) => DFunctor (Postcompose rec f) where
  ffmap f = Postcompose . (composeMap f <<$>>) . getPostcompose
    where composeMap f' = Compose . (f' <$>) . getCompose

instance (DApplicative rec, Applicative f) => DApplicative (Postcompose rec f) where
  fpure x = Postcompose (fpure (Compose (pure x)))
  Postcompose f <<*>> Postcompose x = Postcompose (fliftA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (liftA2 ($$) g y)

instance (DTraversable rec, Traversable f) => DTraversable (Postcompose rec f) where
  fsequenceA (Postcompose x) = fmap Postcompose (ftraverse composeSequenceA x)
    where
      composeSequenceA (Compose y) =
        Compose (fmap Compose (sequenceA (fmap getCompose y)))
