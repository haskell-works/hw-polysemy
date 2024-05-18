module HaskellWorks.Polysemy.Data.Aeson
  ( readJsonFile
  ) where

import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy as LBS

import           Data.Aeson

readJsonFile fp = do
  bs <- LBS.readFile fp
  case eitherDecode bs of
    Left  e -> failWithCustom $ "Failed to parse JSON file: " <> e
    Right v -> return v
