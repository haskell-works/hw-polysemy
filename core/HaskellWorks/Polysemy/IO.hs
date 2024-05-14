module HaskellWorks.Polysemy.IO
  ( readFile
  ) where

import qualified Control.Exception             as CE
import qualified Data.Text                     as Text
import qualified GHC.Stack                     as GHC
import           HaskellWorks.Polysemy.Prelude
import qualified System.IO                     as IO

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

-- | Read the contents of the 'filePath' file.
readFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r String
readFile filePath = GHC.withFrozenCallStack $ do
  info $ "Reading string file: " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ IO.readFile filePath
  fromEither r
