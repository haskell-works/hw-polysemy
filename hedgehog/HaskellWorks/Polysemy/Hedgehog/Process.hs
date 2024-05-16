module HaskellWorks.Polysemy.Hedgehog.Process
  ( defaultExecConfig
  , execFlex
  , execFlexOk
  , execFlexOk'
  , execOk
  , execOk_
  , exec
  , procFlex
  , procFlex'
  , binFlex

  , waitSecondsForProcess
  , waitSecondsForProcessOk

  ) where

import qualified Control.Concurrent                              as IO
import qualified Control.Concurrent.Async                        as IO
import           Data.Monoid                                     (Last (..))
import           GHC.Stack                                       (callStack)
import qualified HaskellWorks.IO.Process                         as IO
import           HaskellWorks.Polysemy.Cabal
import           HaskellWorks.Polysemy.Error.Types
import           HaskellWorks.Polysemy.Hedgehog.Assert
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           HaskellWorks.Polysemy.Hedgehog.Process.Internal
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Polysemy.System.Process

import qualified Data.List                                       as L
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

-- | Configuration for starting a new process.  This is a subset of 'IO.CreateProcess'.
data ExecConfig = ExecConfig
  { execConfigEnv :: Last [(String, String)]
  , execConfigCwd :: Last FilePath
  } deriving (Eq, Generic, Show)

defaultExecConfig :: ExecConfig
defaultExecConfig = ExecConfig
  { execConfigEnv = mempty
  , execConfigCwd = mempty
  }

-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlexOk :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => String
  -> String
  -> [String]
  -> Sem r String
execFlexOk = execFlexOk' defaultExecConfig

execFlexOk' :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> Sem r String
execFlexOk' execConfig pkgBin envBin arguments = withFrozenCallStack $ do
  (exitResult, stdout, stderr) <- execFlex execConfig pkgBin envBin arguments
  case exitResult of
    ExitFailure exitCode -> do
      jot_ $ L.unlines $
        [ "Process exited with non-zero exit-code: " <> show @Int exitCode ]
        <> (if L.null stdout then [] else ["━━━━ stdout ━━━━" , stdout])
        <> (if L.null stderr then [] else ["━━━━ stderr ━━━━" , stderr])
      failMessage callStack "Execute process failed"
    ExitSuccess -> return stdout

-- | Run a process, returning its exit code, its stdout, and its stderr.
-- Contrary to @execFlexOk'@, this function doesn't fail if the call fails.
-- So, if you want to test something negative, this is the function to use.
execFlex :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String -- ^ @pkgBin@: name of the binary to launch via 'cabal exec'
  -> String -- ^ @envBin@: environment variable defining the binary to launch the process, when in Nix
  -> [String]
  -> Sem r (ExitCode, String, String) -- ^ exit code, stdout, stderr
execFlex execConfig pkgBin envBin arguments = withFrozenCallStack $ do
  cp <- procFlex' execConfig pkgBin envBin arguments
  jot_ . ("━━━━ command ━━━━\n" <>) $ case cmdspec cp of
    ShellCommand cmd    -> cmd
    RawCommand cmd args -> cmd <> " " <> L.unwords (argQuote <$> args)

  readCreateProcessWithExitCode cp ""

-- | Execute a process, returning '()'.
execOk_ :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String
  -> [String]
  -> Sem r ()
execOk_ execConfig bin arguments = void $ execOk execConfig bin arguments

-- | Execute a process, returning the stdout. Fail if the call returns
-- with a non-zero exit code. For a version that doesn't fail upon receiving
-- a non-zero exit code, see 'execAny'.
execOk :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String
  -> [String]
  -> Sem r String
execOk execConfig bin arguments = withFrozenCallStack $ do
  (exitResult, stdout, stderr) <- exec execConfig bin arguments
  case exitResult of
    ExitFailure exitCode ->failMessage callStack . L.unlines $
      [ "Process exited with non-zero exit-code: " <> show @Int exitCode ]
      <> (if L.null stdout then [] else ["━━━━ stdout ━━━━" , stdout])
      <> (if L.null stderr then [] else ["━━━━ stderr ━━━━" , stderr])
    ExitSuccess -> return stdout

-- | Execute a process, returning the error code, the stdout, and the stderr.
exec :: ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String -- ^ The binary to launch
  -> [String] -- ^ The binary's arguments
  -> Sem r (ExitCode, String, String) -- ^ exit code, stdout, stderr
exec execConfig bin arguments = withFrozenCallStack $ do
  let cp = (proc bin arguments)
        { env = getLast $ execConfigEnv execConfig
        , cwd = getLast $ execConfigCwd execConfig
        }
  jot_ . ( "━━━━ command ━━━━\n" <>) $ bin <> " " <> L.unwords (argQuote <$> arguments)
  readCreateProcessWithExitCode cp ""

waitSecondsForProcess :: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => Int
  -> ProcessHandle
  -> Sem r (Either TimedOut (Maybe ExitCode))
waitSecondsForProcess seconds hProcess = embed $
  IO.race
    (IO.threadDelay (seconds * 1000000) >> return TimedOut)
    (IO.maybeWaitForProcess hProcess)

-- | Wait a maximum of 'seconds' secons for process to exit.
waitSecondsForProcessOk :: ()
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => Int
  -> ProcessHandle
  -> Sem r (Either TimedOut ExitCode)
waitSecondsForProcessOk seconds hProcess = withFrozenCallStack $ do
  result <- waitSecondsForProcess seconds hProcess
  case result of
    Left TimedOut -> do
      jot_ "Timed out waiting for process to exit"
      return (Left TimedOut)
    Right maybeExitCode -> do
      case maybeExitCode of
        Nothing -> failMessage callStack "No exit code for process"
        Just exitCode -> do
          jot_ $ "Process exited " <> show exitCode
          return (Right exitCode)

-- | Compute the path to the binary given a package name or an environment variable override.
binFlex :: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> Sem r FilePath
  -- ^ Path to executable
binFlex pkg binaryEnv = do
  maybeEnvBin <- lookupEnv binaryEnv
  case maybeEnvBin of
    Just envBin -> return envBin
    Nothing     -> binDist pkg

-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable, an environment variable pointing to the executable,
-- and an argument list.
--
-- The actual executable used will the one specified by the environment variable, but if
-- the environment variable is not defined, it will be found instead by consulting the
-- "plan.json" generated by cabal.  It is assumed that the project has already been
-- configured and the executable has been built.
procFlex :: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> Sem r CreateProcess
  -- ^ Captured stdout
procFlex = procFlex' defaultExecConfig

procFlex' :: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> Sem r CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments = withFrozenCallStack $ do
  bin <- binFlex pkg binaryEnv
  return (proc bin arguments)
    { env = getLast $ execConfigEnv execConfig
    , cwd = getLast $ execConfigCwd execConfig
    -- this allows sending signals to the created processes, without killing the test-suite process
    , create_group = True
    }
