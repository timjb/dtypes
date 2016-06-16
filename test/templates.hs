{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import FTypes.Classes
import FTypes.TH

import Control.Applicative
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))

import Safe (readMay)
import Test.Framework

data Foo a = Bar Int String | Blub () a

makeFRecord ''Foo

ffoo1, ffoo2 :: FFoo Bool Maybe
ffoo1 = FBar Nothing (Just "hallo")
ffoo2 = FBlub (Just ()) (Just True)

newtype Url = Url { unUrl :: String } deriving (Eq, Show)

makeFRecord ''Url

maybeUrl1, maybeUrl2 :: FUrl Maybe
maybeUrl1 = FUrl Nothing
maybeUrl2 = FUrl (Just "http://haskell.org/")

listUrl :: FUrl []
listUrl = ffmap toList maybeUrl1

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
  , personAge = 21
  , personHomepage = Url "http://timbaumann.info/"
  }

makeFRecord ''Person

unparsedMe :: FPerson (Const String)
unparsedMe =
  FPerson
  { fpersonName = Const "Tim Baumann"
  , fpersonAge = Const "21"
  , fpersonHomepage = Const "http://timbaumann.info/"
  }

unitMe :: FPerson (Const ())
unitMe = (\(Const _) -> Const ()) <<$>> unparsedMe

serializedMe :: String
serializedMe = getConst (fsequenceA' unparsedMe)

test_serializedMe = assertEqual serializedMe "Tim Baumann21http://timbaumann.info/"

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

personFormatDesc :: FPerson FieldDesc
personFormatDesc =
  FPerson
  { fpersonName = StringField
  , fpersonAge = IntField
  , fpersonHomepage = UrlField
  }

maybeParsedMe :: Maybe (FPerson Identity)
maybeParsedMe = fsequenceA' (liftFA2 parseField personFormatDesc unparsedMe)

-- TODO: Use Iso to compare for equality
test_maybeParsedMe = do
  assertEqual (fpersonName <$> maybeParsedMe) (Just $ Identity "Tim Baumann")
  assertEqual (fpersonAge <$> maybeParsedMe) (Just $ Identity 21)
  assertEqual (fpersonHomepage <$> maybeParsedMe) (Just $ Identity (Url "http://timbaumann.info/"))

test_osiiso = assertEqual (fosi (fiso me)) me

data SumType
  = MkInt Int
  | MkString String

makeFRecord ''SumType

deriving instance Eq (FSumType Identity)
deriving instance Eq a => Eq (FSumType (Const a))
deriving instance Show (FSumType Identity)
deriving instance Show a => Show (FSumType (Const a))

test_fchoose = do
  assertEqual (fchoose intConstString) (Left (FMkInt (Const "zweiundvierzig")))
  assertEqual (fchoose stringIdentity) (Right (FMkString (Identity "Hallo")))
  where
    intConstString, stringIdentity :: FSumType (Const String :+: Identity)
    intConstString = FMkInt (LeftF (Const "zweiundvierzig"))
    stringIdentity = FMkString (RightF (Identity "Hallo"))

main :: IO ()
main = htfMain htf_thisModulesTests
