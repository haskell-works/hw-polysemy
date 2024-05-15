module HaskellWorks.Polysemy.ByteString.Lazy
  ( readFile
  ) where

import qualified Control.Exception             as CE
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import           HaskellWorks.Polysemy.Prelude

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

-- | Read the contents of the 'filePath' file.
readFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r LBS.ByteString
readFile filePath = withFrozenCallStack $ do
  info $ "Reading lazy bytestring file: " <> T.pack filePath
  r <- embed $ CE.try @IOException $ LBS.readFile filePath
  fromEither r
