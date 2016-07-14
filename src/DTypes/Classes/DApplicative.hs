{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module DTypes.Classes.DApplicative
  ( DApplicative (..)
  , dpureTrafo
  , dliftA1, dliftA2, dliftA3, dliftA4, dliftA5, dliftA6, dliftA7, dliftA8
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

class DFunctor d => DApplicative (d :: (k -> *) -> *) where
  -- axioms (analog to applicative axioms):
  --   dpure idTrafo <<*>> d = r
  --   pureTrafo o <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
  --   pureTrafo f <<**>> dpure x = dpure (f x)
  --   u <*> dpure y = dpure ($$ y) <*> u
  dpure :: (forall (a :: k). f a) -> d f
  (<<*>>) :: d (f ==>> g) -> d f -> d g

  (*>>) :: d f -> d g -> d g
  s *>> t = (wrap1 id <<$ s) <<*>> t
  (<<*) :: d f -> d g -> d f
  (<<*) = dliftA2 const

instance (Applicative f, DApplicative d) => DApplicative (Compose f d) where
  dpure x = Compose (pure (dpure x))
  Compose f <<*>> Compose x = Compose (liftA2 (<<*>>) f x)

dpureTrafo :: DApplicative d => (f ==> g) -> d (f ==>> g)
dpureTrafo f = dpure (TrafoComp f)

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

dliftA1
  :: DFunctor d
  => (forall a. f a -> g a)
  -> d f -> d g
dliftA1 = dfmap

dliftA2
  :: DApplicative d
  => (forall a. f a -> g a -> h a)
  -> d f -> d g -> d h
dliftA2 f s t = (wrap1 <$> f) <<$>> s <<*>> t

dliftA3
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a)
  -> d f -> d g -> d h -> d i
dliftA3 f s t u = (wrap2 <$> f) <<$>> s <<*>> t <<*>> u

dliftA4
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> d f -> d g -> d h -> d i -> d j
dliftA4 f s t u v = (wrap3 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v

dliftA5
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a)
  -> d f -> d g -> d h -> d i -> d j -> d k
dliftA5 f s t u v w = (wrap4 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w

dliftA6
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a)
  -> d f -> d g -> d h -> d i -> d j -> d k -> d l
dliftA6 f s t u v w x =
  (wrap5 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x

dliftA7
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a)
  -> d f -> d g -> d h -> d i -> d j -> d k -> d l -> d m
dliftA7 f s t u v w x y =
  (wrap6 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y

dliftA8
  :: DApplicative d
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a -> n a)
  -> d f -> d g -> d h -> d i -> d j -> d k -> d l -> d m -> d n
dliftA8 f s t u v w x y z =
  (wrap7 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y <<*>> z
