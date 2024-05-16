module HaskellWorks.IO.Process
  ( maybeWaitForProcess
  ) where

import qualified Control.Concurrent       as IO
import           Control.Concurrent.Async
import qualified Control.Concurrent.Async as IO
import qualified Control.Exception        as IO
import           Data.Maybe
import           System.Exit
import           System.IO
import qualified System.Process           as IO

import           Control.Applicative
import           Data.Function
import           Data.Functor
import           GHC.Stack                (HasCallStack, withFrozenCallStack)
import           System.Process

maybeWaitForProcess :: ()
  => ProcessHandle
  -> IO (Maybe ExitCode)
maybeWaitForProcess hProcess =
  IO.catch (fmap Just (IO.waitForProcess hProcess)) $ \(_ :: AsyncCancelled) -> pure Nothing
