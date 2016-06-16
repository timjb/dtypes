{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Classes.FChoice
  ( (:+:) (..)
  , FChoice (..)
  ) where

import FTypes.Classes.FFunctor

-- TODO: import
data (:+:) f g a = LeftF (f a) | RightF (g a)

class FFunctor rec => FChoice (rec :: (k -> *) -> *) where
  fchoose :: rec (f :+: g) -> Either (rec f) (rec g)
