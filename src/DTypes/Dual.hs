{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImpredicativeTypes #-}

module DTypes.Dual
  ( Elim (..)
  , Dual (..)
  , pair, pairElimL, pairElimR
  ) where

import DTypes.Classes

newtype Elim f c a
  = Elim
  { getElim :: f a -> c
  }

contraMapElim :: (f a -> g a) -> Elim g c a -> Elim f c a
contraMapElim trafo (Elim elim) = Elim (elim . trafo)

elimInvolutionIso :: f a -> Elim (Elim f c) c a
elimInvolutionIso val = Elim (\(Elim f) -> f val)

--elimInvolutionOsi :: Elim (Elim f (f a)) (f a) a -> f a
--elimInvolutionOsi (Elim elim) = elim (Elim id)

combineElims :: Elim f c a -> Elim g c a -> Elim (f :+: g) c a
combineElims (Elim leftElim) (Elim rightElim) =
  Elim $ \case
    LeftF l -> leftElim l
    RightF r -> rightElim r

newtype Dual rec f
  = Dual
  { getDual :: forall c. rec (Elim f c) -> c
  }

instance DFunctor rec => DFunctor (Dual rec) where
  dfmap trafo (Dual elim) = Dual (\val -> elim (contraMapElim trafo <<$>> val))

dualInvolutionIso :: DFunctor d => d f -> Dual (Dual d) f
dualInvolutionIso val = Dual $ \(Dual f) -> f (elimInvolutionIso <<$>> val)

pairElimR :: Dual d f -> d (Elim f x) -> x
pairElimR = getDual

pair
  :: DFunctor d
  => (forall a. f a -> g a -> x)
  -> Dual d f
  -> d g
  -> x
pair f d x =
  pairElimR d (Elim . flip f <<$>> x)

pairElimL :: DFunctor d => Dual d (Elim f x) -> d f -> x
pairElimL =
  pair getElim

{-
instance FDistributive rec => FChoice (Dual rec) where
  fchoose (Dual val) = _

instance FChoice rec => FDistributive (Dual rec) where
  fdistribute val = _
-}

{-
dualInvolutionOsi :: Dual (Dual rec) f -> rec f
dualInvolutionOsi (Dual val) = val (Dual (\x -> val (Dual _)))

instance FApplicative rec => FChoice (Dual rec) where
  fzero elim (Dual val) = val (fpure (Elim elim))
  fchoose (Dual val) = _

instance FChoice rec => FApplicative (Dual rec) where
  fpure val = Dual (\s -> fzero (\(Elim elim) -> elim val) s)
  Dual x <<*>> Dual y = Dual (\elim -> _)

instance FApplicative rec => FChoice (Dual rec) where
  fchoose leftCases rightCases (Dual val) =
    val _ --(liftFA2 combineElims leftCases rightCases)
-}

-- :: rec (Elim (Compose (Dual rec) f) c) -> rec (Elim (Diag f) c)

{-
instance FMonad rec => FComonad (Dual rec) where
  fextract (Dual val) elim = val (freturn (Elim elim))
  fduplicate (Dual val) =
    Dual $ \rec -> val _
-}
