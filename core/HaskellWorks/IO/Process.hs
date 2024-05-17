module HaskellWorks.IO.Process
  ( maybeWaitForProcess
  , waitSecondsForProcess
  ) where

import           Control.Concurrent       as IO
import           Control.Concurrent.Async as IO
import qualified Control.Exception        as IO
import           Data.Maybe
import           System.Exit
import           System.IO

import           Control.Applicative
import           Data.Function
import           Data.Functor
import           HaskellWorks.Error.Types
import           HaskellWorks.Prelude
import qualified System.Process           as IO
import           System.Process

maybeWaitForProcess :: ()
  => ProcessHandle
  -> IO (Maybe ExitCode)
maybeWaitForProcess hProcess =
  IO.catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> pure Nothing

waitSecondsForProcess :: ()
  => Int
  -> ProcessHandle
  -> IO (Either TimedOut (Maybe ExitCode))
waitSecondsForProcess seconds hProcess =
  IO.race
    (IO.threadDelay (seconds * 1000000) >> pure (TimedOut "Timed out waiting for process"))
    (maybeWaitForProcess hProcess)
