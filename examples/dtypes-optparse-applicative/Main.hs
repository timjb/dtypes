{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import qualified Options.Applicative as OA
import DTypes.TH (makeDType)
import DTypes.Classes.DTraversable (dsequenceA')
import DTypes.Classes.HasDType (HasDType (..))

-- adapted example from the optparse-applicative README

data Sample
  = Sample
  { hello :: String
  , quiet :: Bool
  }

makeDType ''Sample

sample :: OA.Parser Sample
sample =
  fmap dosi $ dsequenceA' $
  FSample
  { fquiet =
      OA.switch $
      mconcat
      [ OA.long "quiet"
      , OA.help "Whether to be quiet"
      ]
  , fhello =
      OA.strOption $
      mconcat
      [ OA.long "hello"
      , OA.metavar "TARGET"
      , OA.help "Target for the greeting"
      ]
  }

greet :: Sample -> IO ()
greet sample =
  if quiet sample then
    return ()
  else
    putStrLn $ "Hello, " ++ hello sample

main :: IO ()
main =
  OA.execParser opts >>= greet
  where
    opts =
      OA.info (OA.helper <*> sample) $
      mconcat
      [ OA.fullDesc
      , OA.progDesc "Print a greeting for TARGET"
      , OA.header "hello - a test for optparse-applicative"
      ]
