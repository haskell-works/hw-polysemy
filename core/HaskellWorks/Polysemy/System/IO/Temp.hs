module HaskellWorks.Polysemy.System.IO.Temp
  ( createTempDirectory,
    getCanonicalTemporaryDirectory,
  ) where

import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import qualified System.IO.Temp                as IO

createTempDirectory :: ()
  => HasCallStack
  => Member (Embed IO) r
  => FilePath
  -> String
  -> Sem r FilePath
createTempDirectory fp template = withFrozenCallStack $ do
  embed $ IO.createTempDirectory fp template
{-# INLINE createTempDirectory #-}

getCanonicalTemporaryDirectory :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Sem r FilePath
getCanonicalTemporaryDirectory = withFrozenCallStack $ do
  embed IO.getCanonicalTemporaryDirectory
{-# INLINE getCanonicalTemporaryDirectory #-}
