module HaskellWorks.Polysemy.File
  ( JsonDecodeError(..)
  , YamlDecodeError(..)
  , readJsonFile
  , readYamlFile
  ) where

import           Data.Aeson
import qualified Data.Aeson                                        as J
import qualified Data.Yaml                                         as Y
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy        as LBS
import qualified HaskellWorks.Polysemy.Data.Text                   as T
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Error.Types.JsonDecodeError
import           HaskellWorks.Polysemy.Error.Types.YamlDecodeError
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

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
    & onLeft (throw . newJsonDecodeError)

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
    & onLeft (throw . YamlDecodeError . T.pack . Y.prettyPrintParseException)
