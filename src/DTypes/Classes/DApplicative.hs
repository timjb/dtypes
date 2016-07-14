{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DApplicative
  ( DApplicative (..)
  , fpureTrafo
  , fliftA1, fliftA2, fliftA3, fliftA4, fliftA5, fliftA6, fliftA7, fliftA8
  ) where

import DTypes.Classes.DFunctor
import DTypes.Compose
import DTypes.Trafo

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (liftA2)
#else
import Control.Applicative ((<$>), liftA2)
#endif

infixl 4 <<*>>, <<*, *>>

class DFunctor r => DApplicative (r :: (k -> *) -> *) where
  -- axioms (analog to applicative axioms):
  --   fpure idTrafo <<*>> r = r
  --   pureTrafo o <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
  --   pureTrafo f <<**>> fpure x = fpure (f x)
  --   u <*> fpure y = fpure ($$ y) <*> u
  fpure :: (forall (a :: k). f a) -> r f
  (<<*>>) :: r (f ==>> g) -> r f -> r g

  (*>>) :: r f -> r g -> r g
  s *>> t = (wrap1 id <<$ s) <<*>> t
  (<<*) :: r f -> r g -> r f
  (<<*) = fliftA2 const

instance (Applicative f, DApplicative r) => DApplicative (Compose f r) where
  fpure x = Compose (pure (fpure x))
  Compose f <<*>> Compose x = Compose (liftA2 (<<*>>) f x)

fpureTrafo :: DApplicative r => (f ==> g) -> r (f ==>> g)
fpureTrafo f = fpure (TrafoComp f)

wrap1
  :: (f a -> g a)
  -> (f ==>> g) a
wrap1 = TrafoComp

wrap2
  :: (f a -> g a -> h a)
  -> (f ==>> g ==>> h) a
wrap2 f = TrafoComp (wrap1 <$> f)

wrap3
  :: (f a -> g a -> h a -> k a)
  -> (f ==>> g ==>> h ==>> k) a
wrap3 f = TrafoComp (wrap2 <$> f)

wrap4
  :: (f a -> g a -> h a -> i a -> j a)
  -> (f ==>> g ==>> h ==>> i ==>> j) a
wrap4 f = TrafoComp (wrap3 <$> f)

wrap5
  :: (f a -> g a -> h a -> i a -> j a -> k a)
  -> (f ==>> g ==>> h ==>> i ==>> j ==>> k) a
wrap5 f = TrafoComp (wrap4 <$> f)

wrap6
  :: (f a -> g a -> h a -> i a -> j a -> k a -> l a)
  -> (f ==>> g ==>> h ==>> i ==>> j ==>> k ==>> l) a
wrap6 f = TrafoComp (wrap5 <$> f)

wrap7
  :: (f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a)
  -> (f ==>> g ==>> h ==>> i ==>> j ==>> k ==>> l ==>> m) a
wrap7 f = TrafoComp (wrap6 <$> f)

fliftA1
  :: DFunctor r
  => (forall a. f a -> g a)
  -> r f -> r g
fliftA1 = ffmap

fliftA2
  :: DApplicative r
  => (forall a. f a -> g a -> h a)
  -> r f -> r g -> r h
fliftA2 f s t = (wrap1 <$> f) <<$>> s <<*>> t

fliftA3
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a)
  -> r f -> r g -> r h -> r i
fliftA3 f s t u = (wrap2 <$> f) <<$>> s <<*>> t <<*>> u

fliftA4
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> r f -> r g -> r h -> r i -> r j
fliftA4 f s t u v = (wrap3 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v

fliftA5
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a)
  -> r f -> r g -> r h -> r i -> r j -> r k
fliftA5 f s t u v w = (wrap4 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w

fliftA6
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l
fliftA6 f s t u v w x =
  (wrap5 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x

fliftA7
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l -> r m
fliftA7 f s t u v w x y =
  (wrap6 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y

fliftA8
  :: DApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a -> n a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l -> r m -> r n
fliftA8 f s t u v w x y z =
  (wrap7 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y <<*>> z
