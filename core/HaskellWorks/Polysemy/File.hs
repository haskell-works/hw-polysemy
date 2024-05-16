{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Polysemy.File
  ( JsonDecodeError(..)
  , YamlDecodeError(..)
  , readJsonFile
  , readYamlFile
  ) where

import           Data.Aeson
import qualified Data.Aeson                                 as J
import qualified Data.Yaml                                  as Y
import           GHC.Generics
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy as LBS
import qualified HaskellWorks.Polysemy.Data.Text            as T
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

newtype JsonDecodeError = JsonDecodeError { message :: String }
  deriving (Eq, Generic, Show)

newtype YamlDecodeError = YamlDecodeError { message :: String }
  deriving (Eq, Generic, Show)

-- | Read the 'filePath' file as JSON. Use @readJsonFile \@'Value'@ to decode into 'Value'.
readJsonFile :: forall a r. ()
  => FromJSON a
  => HasCallStack
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r a
readJsonFile filePath = withFrozenCallStack $ do
  info $ "Reading JSON file: " <> T.pack filePath
  contents <- LBS.readFile filePath
  J.eitherDecode contents
    & onLeft (throw . JsonDecodeError)


-- | Read the 'filePath' file as YAML.
readYamlFile :: forall a r. ()
  => FromJSON a
  => HasCallStack
  => Member (Error IOException) r
  => Member (Error JsonDecodeError) r
  => Member (Error YamlDecodeError) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r a
readYamlFile filePath = withFrozenCallStack $ do
  info $ "Reading YAML file: " <> T.pack filePath
  contents <- LBS.toStrict <$> LBS.readFile filePath
  Y.decodeEither' contents
    & onLeft (throw . YamlDecodeError . Y.prettyPrintParseException)
