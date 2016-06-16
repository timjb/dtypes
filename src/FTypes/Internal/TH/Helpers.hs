{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module FTypes.Internal.TH.Helpers
  ( destructure
  , appEs
  , apAppEs
  , liftAppEs
  , nameFromTyVarBndr
  , conAppsT
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif

import Language.Haskell.TH

-- | Generates `case $val of { $pat -> $exp }`
destructure :: ExpQ -> PatQ -> ExpQ -> ExpQ
destructure val pat expr = caseE val [match pat (normalB expr) []]

-- | Generates `f arg1 ... argn`
appEs :: ExpQ -> [ExpQ] -> ExpQ
appEs = foldl appE

-- | Generates `f <*> arg1 <*> ... <*> argn`
apAppEs :: ExpQ -> [ExpQ] -> ExpQ
apAppEs = foldl (\g y -> [e| $g <*> $y |])

-- | Generates `f <$> arg1 <*> ... <*> argn`
liftAppEs :: ExpQ -> [ExpQ] -> ExpQ
liftAppEs x args =
  case args of
    [] -> [e| pure $x |]
    firstArg:nextArgs -> apAppEs [e| $x <$> $firstArg |] nextArgs

-- | Extract the name from a TyVarBndr.
nameFromTyVarBndr :: TyVarBndr -> Name
nameFromTyVarBndr bndr =
  case bndr of
    PlainTV name -> name
    KindedTV name _kind -> name

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)
