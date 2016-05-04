{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Main (main) where

import FRecords.TH

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

data Person
  = Person
  { personName :: String
  , personAge :: Int
  , personHomepage :: Url
  }

makeFRecord ''Person

data Const x a = Const { unConst :: x }

unparsedMe :: FPerson (Const String)
unparsedMe
  = FPerson
  { fpersonName = Const "Tim Baumann"
  , fpersonAge = Const "21"
  , fpersonHomepage = Const "http://timbaumann.info/"
  }

main :: IO ()
main = return ()