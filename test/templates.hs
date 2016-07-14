{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import DTypes.Classes
import DTypes.TH

import Control.Applicative
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))

import Safe (readMay)
import Test.Framework

data Foo a
  = Bar Int String
  | Blub () a

makeDType ''Foo

ffoo1, ffoo2 :: DFoo Bool Maybe
ffoo1 = DBar Nothing (Just "hallo")
ffoo2 = DBlub (Just ()) (Just True)

newtype Url
  = Url
  { unUrl :: String
  } deriving (Eq, Show)

makeDType ''Url

maybeUrl1, maybeUrl2 :: DUrl Maybe
maybeUrl1 = DUrl Nothing
maybeUrl2 = DUrl (Just "http://haskell.org/")

listUrl :: DUrl []
listUrl = dfmap toList maybeUrl1

data Person
  = Person
  { personName :: String
  , personAge :: Int
  , personHomepage :: Url
  } deriving (Eq, Show)

me :: Person
me =
  Person
  { personName = "Tim Baumann"
  , personAge = 22
  , personHomepage = Url "http://timbaumann.info/"
  }

makeDType ''Person

unparsedMe :: DPerson (Const String)
unparsedMe =
  DPerson
  { dpersonName = Const "Tim Baumann"
  , dpersonAge = Const "22"
  , dpersonHomepage = Const "http://timbaumann.info/"
  }

unitMe :: DPerson (Const ())
unitMe = (\(Const _) -> Const ()) <<$>> unparsedMe

serializedMe :: String
serializedMe = getConst (dsequenceA' unparsedMe)

test_serializedMe = assertEqual serializedMe "Tim Baumann22http://timbaumann.info/"

data FieldDesc a where
  StringField :: FieldDesc String
  IntField :: FieldDesc Int
  UrlField :: FieldDesc Url

parseField :: FieldDesc a -> Const String a -> Maybe a
parseField fieldDesc (Const str) =
  case fieldDesc of
    StringField -> Just str
    IntField -> readMay str
    UrlField -> Just (Url str) -- fake-todo: validation

personFormatDesc :: DPerson FieldDesc
personFormatDesc =
  DPerson
  { dpersonName = StringField
  , dpersonAge = IntField
  , dpersonHomepage = UrlField
  }

maybeParsedMe :: Maybe (DPerson Identity)
maybeParsedMe = dsequenceA' (dliftA2 parseField personFormatDesc unparsedMe)

-- TODO: Use Iso to compare for equality
test_maybeParsedMe = do
  assertEqual (dpersonName <$> maybeParsedMe) (Just $ Identity "Tim Baumann")
  assertEqual (dpersonAge <$> maybeParsedMe) (Just $ Identity 22)
  assertEqual (dpersonHomepage <$> maybeParsedMe) (Just $ Identity (Url "http://timbaumann.info/"))

test_osiiso = assertEqual (dosi (diso me)) me

data SumType
  = MkInt Int
  | MkString String

makeDType ''SumType

deriving instance Eq (DSumType Identity)
deriving instance Show (DSumType Identity)

-- Once we drop compatibility for GHC 7.8, we can use the following two lines:
--deriving instance Eq a => Eq (DSumType (Const a))
--deriving instance Show a => Show (DSumType (Const a))

instance Eq a => Eq (DSumType (Const a)) where
  DMkInt (Const x) == DMkInt (Const y) = x == y
  DMkString (Const x) == DMkString (Const y) = x == y
  _ == _ = False

instance Show a => Show (DSumType (Const a)) where
  show (DMkInt (Const x)) = "DMkInt (Const " ++ showsPrec 9 x ")"
  show (DMkString (Const x)) = "DMkString (Const " ++ showsPrec 9 x ")"

test_dchoose = do
  assertEqual (dchoose intConstString) (Left (DMkInt (Const "zweiundvierzig")))
  assertEqual (dchoose stringIdentity) (Right (DMkString (Identity "Hallo")))
  where
    intConstString, stringIdentity :: DSumType (Const String :+: Identity)
    intConstString = DMkInt (LeftF (Const "zweiundvierzig"))
    stringIdentity = DMkString (RightF (Identity "Hallo"))

main :: IO ()
main = htfMain htf_thisModulesTests
