module HaskellWorks.Polysemy.System.Process
  ( IO.CreateProcess(..),
    IO.CmdSpec(..),
    IO.StdStream(..),
    Handle,
    ProcessHandle,
    ExitCode(..),
    FD,
    Pid,
    createProcess,
    createProcess_,
    IO.shell,
    IO.proc,
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
    cleanupProcess,
    getPid,
    getCurrentPid,
    interruptProcessGroupOf,
    createPipe,
    createPipeFd,
    runProcess,
    runCommand,
    runInteractiveProcess,
    runInteractiveCommand,
    system,
    rawSystem,

    waitSecondsForProcess,

  ) where

import qualified Control.Exception             as CE
import           HaskellWorks.Error
import           HaskellWorks.Error.Types
import qualified HaskellWorks.IO.Process       as IO
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log
import           System.Exit                   (ExitCode (..))
import           System.IO                     (Handle)
import           System.Posix.Internals        (FD)
import qualified System.Process                as IO
import           System.Process                (Pid, ProcessHandle)

createProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => IO.CreateProcess
  -> Sem r (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  r <- embed $ CE.try @IOException $ IO.createProcess cp
  fromEither r

createProcess_ :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> IO.CreateProcess
  -> Sem r (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ cmd cp = do
  r <- embed $ CE.try @IOException $ IO.createProcess_ cmd cp
  fromEither r

callProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> [String]
  -> Sem r ()
callProcess cmd args = do
  r <- embed $ CE.try @IOException $ IO.callProcess cmd args
  fromEither r

callCommand :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r ()
callCommand cmd = do
  r <- embed $ CE.try @IOException $ IO.callCommand cmd
  fromEither r

spawnProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> [String]
  -> Sem r ProcessHandle
spawnProcess cmd args = do
  r <- embed $ CE.try @IOException $ IO.spawnProcess cmd args
  fromEither r

spawnCommand :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r ProcessHandle
spawnCommand cmd = do
  r <- embed $ CE.try @IOException $ IO.spawnCommand cmd
  fromEither r

readCreateProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => IO.CreateProcess
  -> String
  -> Sem r String
readCreateProcess cp input = do
  r <- embed $ CE.try @IOException $ IO.readCreateProcess cp input
  fromEither r

readProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> [String]
  -> String
  -> Sem r String
readProcess cmd args input = do
  r <- embed $ CE.try @IOException $ IO.readProcess cmd args input
  fromEither r

readCreateProcessWithExitCode :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => IO.CreateProcess
  -> String
  -> Sem r (ExitCode, String, String)
readCreateProcessWithExitCode cp input = do
  r <- embed $ CE.try @IOException $ IO.readCreateProcessWithExitCode cp input
  fromEither r

readProcessWithExitCode :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> [String]
  -> String
  -> Sem r (ExitCode, String, String)
readProcessWithExitCode cmd args input = do
  r <- embed $ CE.try @IOException $ IO.readProcessWithExitCode cmd args input
  fromEither r

cleanupProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> Sem r ()
cleanupProcess (mIn, mOut, mErr, ph) = do
  r <- embed $ CE.try @IOException $ IO.cleanupProcess (mIn, mOut, mErr, ph)
  fromEither r

getPid :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => ProcessHandle
  -> Sem r (Maybe Pid)
getPid ph = do
  r <- embed $ CE.try @IOException $ IO.getPid ph
  fromEither r

getCurrentPid :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Sem r Pid
getCurrentPid = do
  r <- embed $ CE.try @IOException $ IO.getCurrentPid
  fromEither r

interruptProcessGroupOf :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => ProcessHandle
  -> Sem r ()
interruptProcessGroupOf ph = do
  r <- embed $ CE.try @IOException $ IO.interruptProcessGroupOf ph
  fromEither r

createPipe :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Sem r (Handle, Handle)
createPipe = do
  r <- embed $ CE.try @IOException $ IO.createPipe
  fromEither r

createPipeFd :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Sem r (FD, FD)
createPipeFd = do
  r <- embed $ CE.try @IOException $ IO.createPipeFd
  fromEither r

runProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> Sem r ProcessHandle
runProcess cmd args mbStdIn mbEnv mbCwd mbStdOut mbStdErr = do
  r <- embed $ CE.try @IOException $ IO.runProcess cmd args mbStdIn mbEnv mbCwd mbStdOut mbStdErr
  fromEither r

runCommand :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r ProcessHandle
runCommand cmd = do
  r <- embed $ CE.try @IOException $ IO.runCommand cmd
  fromEither r

runInteractiveProcess :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> Sem r (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess cmd args mbCwd mbEnv = do
  r <- embed $ CE.try @IOException $ IO.runInteractiveProcess cmd args mbCwd mbEnv
  fromEither r

runInteractiveCommand :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommand cmd = do
  r <- embed $ CE.try @IOException $ IO.runInteractiveCommand cmd
  fromEither r

system :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r ExitCode
system cmd = do
  r <- embed $ CE.try @IOException $ IO.system cmd
  fromEither r

rawSystem :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> [String]
  -> Sem r ExitCode
rawSystem cmd args = do
  r <- embed $ CE.try @IOException $ IO.rawSystem cmd args
  fromEither r

waitSecondsForProcess :: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member (Error TimedOut) r
  => Member Log r
  => Int
  -> ProcessHandle
  -> Sem r (Maybe ExitCode)
waitSecondsForProcess seconds hProcess =
  embed (IO.waitSecondsForProcess seconds hProcess)
    & onLeftM @TimedOut throw
