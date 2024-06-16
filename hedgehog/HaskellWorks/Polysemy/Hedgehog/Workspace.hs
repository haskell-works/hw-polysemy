module HaskellWorks.Polysemy.Hedgehog.Workspace
  ( PackagePath(..),
    ProjectRoot(..),
    Workspace(..),
    workspace,
    moduleWorkspace,
    findCabalProjectDir,
  ) where

import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Hedgehog.Assert
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           HaskellWorks.Polysemy.Hedgehog.Workspace.Types
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.Stack
import           HaskellWorks.Polysemy.System.Directory
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Polysemy.System.IO.Temp
import           Polysemy
import           Polysemy.Log
import           Polysemy.Reader
import           System.Info

import qualified HaskellWorks.Polysemy.System.IO                as PIO

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => HasCallStack
  => FilePath
  -> Sem (Reader Workspace : r) ()
  -> Sem r ()
workspace prefixPath f = withFrozenCallStack $ do
  systemTemp <- getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- lookupEnv "KEEP_WORKSPACE"
  ws <- createTempDirectory systemTemp $ prefixPath <> "-test"
  jot_ $ "Workspace: " <> ws
  PIO.writeFile (ws </> "module") callerModuleName
    & trapFail @IOException
  runReader (Workspace ws) f
  when (os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    removePathForcibly ws
      & trapFail @IOException

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
  => HasCallStack
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => String
  -> Sem (Reader Workspace : r) ()
  -> Sem r ()
moduleWorkspace prefix f = withFrozenCallStack $
  workspace (prefix <> "-" <> callerModuleName) f

-- | Compute the project base.  This will be the first parent directory that contains
-- the `cabal.project` file.
-- This should should point to the root directory of the Github project checkout.
findCabalProjectDir :: ()
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
findCabalProjectDir dir = do
  atBase <- doesFileExist (dir </> "cabal.project")
    & trap_ @IOException (pure False)
  if atBase
    then return dir
    else do
      let up = dir </> ".."
      upExist <- doesDirectoryExist up
        & trap_ @IOException (pure False)
      if upExist
        then findCabalProjectDir up
        else embed $ fail "Could not detect project base directory (containing cabal.project)"
