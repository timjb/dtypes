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

newtype Postcompose d f g
  = Postcompose
  { getPostcompose :: d (Compose f g)
  }

instance (DFunctor d, Functor f) => DFunctor (Postcompose d f) where
  dfmap f = Postcompose . (composeMap f <<$>>) . getPostcompose
    where composeMap f' = Compose . (f' <$>) . getCompose

instance (DApplicative d, Applicative f) => DApplicative (Postcompose d f) where
  dpure x = Postcompose (dpure (Compose (pure x)))
  Postcompose f <<*>> Postcompose x = Postcompose (dliftA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (liftA2 ($$) g y)

instance (DTraversable d, Traversable f) => DTraversable (Postcompose d f) where
  dsequenceA (Postcompose x) = fmap Postcompose (dtraverse composeSequenceA x)
    where
      composeSequenceA (Compose y) =
        Compose (fmap Compose (sequenceA (fmap getCompose y)))
