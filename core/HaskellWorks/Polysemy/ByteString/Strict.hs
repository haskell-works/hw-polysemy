module HaskellWorks.Polysemy.ByteString.Strict
  ( readFile
  ) where

import qualified Control.Exception             as CE
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
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
  -> Sem r ByteString
readFile filePath = withFrozenCallStack $ do
  info $ "Reading bytestring file: " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ BS.readFile filePath
  fromEither r
