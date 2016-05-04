{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Main (main) where

import FRecords.TH

data Foo a = Bar Int String | Blub () a

makeFRecord ''Foo

ffoo1, ffoo2 :: FFoo Bool Maybe
ffoo1 = FBar Nothing (Just "hallo")
ffoo2 = FBlub (Just ()) (Just True)

main :: IO ()
main = return ()