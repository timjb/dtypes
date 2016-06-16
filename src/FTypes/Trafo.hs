{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Trafo
  ( type Trafo
  , type (==>)
  , TrafoComp (..)
  , type (==>>)
  , ($$)
  ) where

infixr 2 ==>
infixr 2 ==>>

-- | Polymorphic family of functions between f and g.
-- If f and g are both functors, this is the type of natural transformations between them.
type Trafo (f :: k -> *) (g :: k -> *)
  = forall (a :: k). f a -> g a

type f ==> g = Trafo f g

newtype TrafoComp f g a
  = TrafoComp { unTrafoComp :: f a -> g a }

type (==>>) f g = TrafoComp f g

{-
-- | Identity transformation
idTrafo :: TrafoComp f f a
idTrafo = TrafoComp id

-- compose trafos
o :: TrafoComp g h a -> TrafoComp f g a -> TrafoComp f h a
o (TrafoComp g) (TrafoComp f) = TrafoComp (g . f)
-}

-- | Apply a transformation
($$) :: TrafoComp f g a -> f a -> g a
($$) = unTrafoComp
