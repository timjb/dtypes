{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module FRecords.Classes
  ( Trafo
  --, (==>)
  , FFunctor (..)
  , (<<$>>)
  , TrafoComp (..)
  , ($$)
  --, (==>>)
  , FApplicative (..)
  , fpureTrafo
  , liftFA1, liftFA2, liftFA3, liftFA4, liftFA5, liftFA6, liftFA7, liftFA8
  , FTraversable (..)
  , Compose (..) -- TODO
  , ModifiedRec (..)
  , FlipModifiedRec (..)
  ) where

import Data.Functor.Identity (Identity (..))
import Control.Applicative (liftA2)

-- TODO: instance for Const

infixr 2 ==>
infixr 2 ==>>
infixl 4 <<$>>, <<$, <<*>>, <<*, *>>

-- | Polymorphic family of functions between f and g.
-- If f and g are both functors, this is the type of natural transformations between them.
type Trafo (f :: k -> *) (g :: k -> *)
  = forall (a :: k). f a -> g a

type f ==> g = Trafo f g

-- TODO: find a better name
class FFunctor (rec :: (k -> *) -> *) where
  -- axioms:
  -- ffmap id = id
  -- ffmap (f . g) = fmap1 f . fmap1 g
  ffmap :: (f ==> g) -> rec f -> rec g

  -- | Replace all locations in the input with the same value.
  -- The default definition is @'fmap' . 'const'@, but this may be
  -- overridden with a more efficient version.
  (<<$) :: (forall a. g a) -> rec f -> rec g
  x <<$ r = ffmap (const x) r

-- synonym
(<<$>>) :: FFunctor rec => (f ==> g) -> rec f -> rec g
(<<$>>) = ffmap

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

class FFunctor rec => FApplicative (rec :: (k -> *) -> *) where
  -- axioms (analog to applicative axioms):
  --   fpure idTrafo <<*>> rec = rec
  --   pureTrafo o <<*>> u <<*>> v <<*>> w = u <<*>> (v <<*>> w)
  --   pureTrafo f <<**>> fpure x = fpure (f x)
  --   u <*> fpure y = fpure ($$ y) <*> u
  fpure :: (forall (a :: k). f a) -> rec f
  (<<*>>) :: rec (f ==>> g) -> rec f -> rec g

  (*>>) :: rec f -> rec g -> rec g
  s *>> t = (wrap1 id <<$ s) <<*>> t
  (<<*) :: rec f -> rec g -> rec f
  (<<*) = liftFA2 const

fpureTrafo :: FApplicative rec => (f ==> g) -> rec (f ==>> g)
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

class FFunctor rec => FTraversable (rec :: (* -> *) -> *) where
  {-# MINIMAL ftraverse | fsequenceA #-}
  ftraverse :: Applicative g => (f ==> Compose g h) -> rec f -> g (rec h)
  ftraverse f = fsequenceA . ffmap f
  fsequenceA :: Applicative g => rec (Compose g h) -> g (rec h)
  fsequenceA = ftraverse id
  ftraverse' :: Applicative g => (f ==> g) -> rec f -> g (rec Identity)
  ftraverse' f = ftraverse (Compose . fmap Identity . f)
  fsequenceA' :: Applicative f => rec f -> f (rec Identity)
  fsequenceA' = fsequenceA . ffmap (Compose . fmap Identity)
  -- TODO: more functions

{-
recFoldMap
  :: (Monoid m, FTraversable rec)
  => (forall a. f a -> m) -> rec f -> m
recFoldMap f = getConst . recTraverse (Const . f)

recFold
  :: (Monoid m, FTraversable rec)
  => rec (Const m) -> m
recFold = getConst . recSequenceA

recToList
  :: FTraversable rec
  => rec (Const a) -> [a]
recToList = recFoldMap (pure . getConst)
-}

-- TODO: import from the Kmettiverse
newtype Compose f g a = Compose { unCompose :: f (g a) }

-- TODO: find a new name
newtype ModifiedRec rec f g = ModifiedRec { unModifiedRec :: rec (Compose f g) }

instance (FFunctor rec, Functor f) => FFunctor (ModifiedRec rec f) where
  ffmap f (ModifiedRec x) = ModifiedRec (composeMap <<$>> x)
    where composeMap (Compose y) = Compose (fmap f y)
      
instance (FApplicative rec, Applicative f) => FApplicative (ModifiedRec rec f) where
  fpure x = ModifiedRec (fpure (Compose (pure x)))
  ModifiedRec f <<*>> ModifiedRec x = ModifiedRec (liftFA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (liftA2 ($$) g y)
      
instance (FTraversable rec, Traversable f) => FTraversable (ModifiedRec rec f) where
  fsequenceA (ModifiedRec x) = fmap ModifiedRec (ftraverse composeSequenceA x)
    where
      composeSequenceA (Compose y) =
        Compose (fmap Compose (sequenceA (fmap unCompose y)))

newtype FlipModifiedRec rec f g = FlipModifiedRec { unFlipModifiedRec :: rec (Compose g f) }

instance FFunctor rec => FFunctor (FlipModifiedRec rec f) where
  ffmap f (FlipModifiedRec x) = FlipModifiedRec (ffmap composeMap x)
    where composeMap (Compose y) = Compose (f y)

instance FApplicative rec => FApplicative (FlipModifiedRec rec f) where
  fpure x = FlipModifiedRec (fpure (Compose x))
  FlipModifiedRec f <<*>> FlipModifiedRec x =
    FlipModifiedRec (liftFA2 composeAp f x)
    where composeAp (Compose g) (Compose y) = Compose (g $$ y)

instance FTraversable rec => FTraversable (FlipModifiedRec rec f) where
  fsequenceA (FlipModifiedRec x) = FlipModifiedRec <$> ftraverse assoc x
    where
      assoc :: Functor f => Compose (Compose f g) h a -> Compose f (Compose g h) a
      assoc (Compose (Compose y)) = Compose (fmap Compose y) -- TODO: optimize
