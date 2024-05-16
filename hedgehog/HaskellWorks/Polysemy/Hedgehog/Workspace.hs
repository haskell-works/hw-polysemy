module HaskellWorks.Polysemy.Hedgehog.Workspace
  ( workspace
  , moduleWorkspace
  ) where

import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.Stack
import           HaskellWorks.Polysemy.System.Directory
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Polysemy.System.IO.Temp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log
import           System.Info

import qualified HaskellWorks.Polysemy.System.IO          as PIO

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: ()
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => HasCallStack
  => FilePath
  -> (FilePath -> Sem r ())
  -> Sem r ()
workspace prefixPath f = withFrozenCallStack $ do
  systemTemp <- getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- lookupEnv "KEEP_WORKSPACE"
  ws <- createTempDirectory systemTemp $ prefixPath <> "-test"
  jot_ $ "Workspace: " <> ws
  PIO.writeFile (ws </> "module") callerModuleName
  f ws
  when (os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    removePathForcibly ws

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
--
-- The 'prefix' argument should not contain directory delimeters.
moduleWorkspace ::  ()
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> (FilePath -> Sem r ())
  -> Sem r ()
moduleWorkspace prefix f = withFrozenCallStack $
  workspace (prefix <> "-" <> callerModuleName) f
