{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module DTypes.Compose
  ( Compose (..)
  ) where

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative ((<$>), Applicative (..), liftA2)
#endif

-- `Compose` from `Data.Functor.Compose` is not good enough because it is not poly-kinded

newtype Compose (f :: k -> *) (g :: l -> k) (x :: l)
  = Compose
  { getCompose :: f (g x)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose v) = Compose (fmap f <$> v)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
