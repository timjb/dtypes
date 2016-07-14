{-# LANGUAGE PolyKinds #-}

module FTypes.Compose
  ( Compose (..)
  ) where

import Control.Applicative (liftA2)

-- `Compose` from `Data.Functor.Compose` is not good enough because it is not poly-kinded

newtype Compose (f :: k -> *) (g :: l -> k) (x :: l)
  = Compose
  { getCompose :: f (g x)
  }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose v) = Compose (fmap (fmap f) v)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
