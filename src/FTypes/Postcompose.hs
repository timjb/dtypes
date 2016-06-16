{-# LANGUAGE CPP #-}

module FTypes.Postcompose
  ( Postcompose (..)
  ) where

import FTypes.Classes
import FTypes.Compose
import FTypes.Trafo

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative (Applicative (..), (<$>), liftA2)
import Data.Traversable (Traversable (..))
#endif

newtype Postcompose r f g
  = Postcompose { getPostcompose :: r (Compose f g) }

instance (FFunctor rec, Functor f) => FFunctor (Postcompose rec f) where
  ffmap f = Postcompose . (composeMap f <<$>>) . getPostcompose
    where composeMap f' = Compose . (f' <$>) . getCompose

instance (FApplicative rec, Applicative f) => FApplicative (Postcompose rec f) where
  fpure x = Postcompose (fpure (Compose (pure x)))
  Postcompose f <<*>> Postcompose x = Postcompose (liftFA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (liftA2 ($$) g y)

instance (FTraversable rec, Traversable f) => FTraversable (Postcompose rec f) where
  fsequenceA (Postcompose x) = fmap Postcompose (ftraverse composeSequenceA x)
    where
      composeSequenceA (Compose y) =
        Compose (fmap Compose (sequenceA (fmap getCompose y)))
