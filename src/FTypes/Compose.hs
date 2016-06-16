{-# LANGUAGE PolyKinds #-}

module FTypes.Compose
  ( Compose (..)
  ) where

-- `Compose` from `Data.Functor.Compose` is not good enough because it is not poly-kinded

newtype Compose (f :: k -> *) (g :: l -> k) (x :: l)
  = Compose { getCompose :: f (g x) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose v) = Compose (fmap (fmap f) v)
