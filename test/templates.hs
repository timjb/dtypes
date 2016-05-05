{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Main (main) where

import FRecords.Classes
import FRecords.TH

import Control.Applicative (Const (..))
import Data.Foldable (toList)

import Test.Framework

data Foo a = Bar Int String | Blub () a

makeFRecord ''Foo

ffoo1, ffoo2 :: FFoo Bool Maybe
ffoo1 = FBar Nothing (Just "hallo")
ffoo2 = FBlub (Just ()) (Just True)

newtype Url = Url { unUrl :: String }

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
  }

makeFRecord ''Person

unparsedMe :: FPerson (Const String)
unparsedMe
  = FPerson
  { fpersonName = Const "Tim Baumann"
  , fpersonAge = Const "21"
  , fpersonHomepage = Const "http://timbaumann.info/"
  }

unitMe :: FPerson (Const ())
unitMe = (\(Const _) -> Const ()) <<$>> unparsedMe

serializedMe :: String
serializedMe = getConst (fsequenceA' unparsedMe)

test_serializedMe = assertEqual serializedMe "Tim Baumann21http://timbaumann.info/"

main :: IO ()
main = htfMain htf_thisModulesTests