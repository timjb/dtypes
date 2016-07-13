{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main
  ( main
  ) where

import qualified Options.Applicative as OA
import FTypes.TH (makeFType)
import FTypes.Classes.FTraversable (fsequenceA')
import FTypes.Classes.HasFType (HasFType (..))

-- adapted example from the optparse-applicative README

data Sample
  = Sample
  { hello :: String
  , quiet :: Bool
  }

makeFType ''Sample

sample :: OA.Parser Sample
sample =
  fmap fosi $ fsequenceA' $
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
