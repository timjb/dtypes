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

class DFunctor d => DChoice (d :: (k -> *) -> *) where
  dchoose :: d (f :+: g) -> Either (d f) (d g)
