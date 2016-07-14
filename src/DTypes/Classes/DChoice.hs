{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DChoice
  ( (:+:) (..)
  , DChoice (..)
  ) where

import DTypes.Classes.DFunctor

-- TODO: import
data (:+:) f g a = LeftF (f a) | RightF (g a)

class DFunctor rec => DChoice (rec :: (k -> *) -> *) where
  fchoose :: rec (f :+: g) -> Either (rec f) (rec g)
