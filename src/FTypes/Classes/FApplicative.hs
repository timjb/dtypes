{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module FTypes.Classes.FApplicative
  ( FApplicative (..)
  , fpureTrafo
  , liftFA1, liftFA2, liftFA3, liftFA4, liftFA5, liftFA6, liftFA7, liftFA8
  ) where

import FTypes.Classes.FFunctor
import FTypes.Trafo

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

infixl 4 <<*>>, <<*, *>>

class FFunctor r => FApplicative (r :: (k -> *) -> *) where
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
  (<<*) = liftFA2 const

fpureTrafo :: FApplicative r => (f ==> g) -> r (f ==>> g)
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

liftFA1
  :: FFunctor r
  => (forall a. f a -> g a)
  -> r f -> r g
liftFA1 = ffmap

liftFA2
  :: FApplicative r
  => (forall a. f a -> g a -> h a)
  -> r f -> r g -> r h
liftFA2 f s t = (wrap1 <$> f) <<$>> s <<*>> t

liftFA3
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a)
  -> r f -> r g -> r h -> r i
liftFA3 f s t u = (wrap2 <$> f) <<$>> s <<*>> t <<*>> u

liftFA4
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> r f -> r g -> r h -> r i -> r j
liftFA4 f s t u v = (wrap3 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v

liftFA5
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a)
  -> r f -> r g -> r h -> r i -> r j -> r k
liftFA5 f s t u v w = (wrap4 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w

liftFA6
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l
liftFA6 f s t u v w x =
  (wrap5 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x

liftFA7
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l -> r m
liftFA7 f s t u v w x y =
  (wrap6 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y

liftFA8
  :: FApplicative r
  => (forall a. f a -> g a -> h a -> i a -> j a -> k a -> l a -> m a -> n a)
  -> r f -> r g -> r h -> r i -> r j -> r k -> r l -> r m -> r n
liftFA8 f s t u v w x y z =
  (wrap7 <$> f) <<$>> s <<*>> t <<*>> u <<*>> v <<*>> w <<*>> x <<*>> y <<*>> z
