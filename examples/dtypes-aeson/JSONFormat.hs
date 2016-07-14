{-# LANGUAGE FlexibleContexts #-}

module JSONFormat where

import DTypes

import Control.Applicative (Const (..))
import Data.Functor.Identity (Identity (..))

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

data JSONFormat x
  = JSONFormat
  { toJSON :: x -> A.Value
  , parseJSON :: A.Value -> A.Parser x
  }

defaultFormat :: (A.ToJSON x, A.FromJSON x) => JSONFormat x
defaultFormat =
  JSONFormat
    { toJSON = A.toJSON
    , parseJSON = A.parseJSON
    }

data JSONField x
  = JSONField
  { fieldName :: T.Text
  , fieldFormat :: JSONFormat x
  }

field :: (A.ToJSON x, A.FromJSON x) => T.Text -> JSONField x
field name =
  JSONField
    { fieldName = name
    , fieldFormat = defaultFormat
    }

serializeField :: A.KeyValue kv => JSONField x -> x -> kv
serializeField field val =
  fieldName field .= toJSON (fieldFormat field) val

parseField :: JSONField x -> A.Object -> A.Parser x
parseField field obj = do
  jsonVal <- obj .: fieldName field
  parseJSON (fieldFormat field) jsonVal

objectFormat
  :: (HasDType o, DApplicative (DType o), DTraversable (DType o))
  => String
  -> DType o JSONField
  -> JSONFormat o
objectFormat name fields =
  JSONFormat
    { toJSON = serialize
    , parseJSON = parse
    }
  where
    serialize val =
      A.object $
      dtoList $
      dliftA2 (\field (Identity val) -> Const (serializeField field val)) fields $
      diso val
    parse =
      A.withObject name $ \obj ->
        dosi <$> dtraverse' (\field -> parseField field obj) fields
