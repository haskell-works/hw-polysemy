{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Polysemy.Amazonka
  ( AwsLogEntry(..),
    runReaderAwsEnvDiscover,
    sendAws,
    interpretDataLogAwsLogEntryToLog,
    interpretDataLogAwsLogEntryToLogWith,
    interpretDataLogAwsLogEntryLocalToLogWith,
    awsLogLevelToSeverity,
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Typeable

import qualified Amazonka                                          as AWS
import qualified Control.Concurrent.STM                            as STM
import           Control.Lens
import           Data.Binary.Builder                               (Builder)
import qualified Data.Binary.Builder                               as B
import           Data.Generics.Product.Any
import qualified Data.List                                         as L
import qualified Data.Text.Lazy                                    as LT
import qualified Data.Text.Lazy.Encoding                           as LT
import           HaskellWorks.Polysemy.Control.Concurrent.STM.TVar
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Internal.Tactics                         (liftT)
import           Polysemy.Log
import qualified Polysemy.Log.Effect.DataLog                       as Log
import           Polysemy.Reader
import           Polysemy.Resource
import qualified System.IO                                         as IO

data AwsLogEntry = AwsLogEntry
  { logLevel :: AWS.LogLevel
  , builder  :: Builder
  }
  deriving (Generic, Show, Typeable)

runReaderAwsEnvDiscover :: ()
  => Member (Embed IO) r
  => Sem (Reader AWS.Env : r) a
  -> Sem r a
runReaderAwsEnvDiscover f = do
  logger' <- embed $ AWS.newLogger AWS.Debug IO.stdout
  discoveredAwsEnv <- embed $ AWS.newEnv AWS.discover
  let awsEnv = discoveredAwsEnv { AWS.logger = logger' }
  runReader awsEnv f

sendAws :: ()
  => AWS.AWSRequest a
  => Member (DataLog AwsLogEntry) r
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Reader AWS.Env) r
  => Member Resource r
  => MonadIO m
  => Typeable (AWS.AWSResponse a)
  => Typeable a
  => a
  -> Sem r (AWS.AWSResponse a)
sendAws req = do
  tStack <- newTVarIO @[AwsLogEntry] []
  envAws0 <- ask @AWS.Env

  let logger ::  AWS.LogLevel -> Builder -> IO ()
      logger ll b = STM.atomically $ STM.modifyTVar tStack (AwsLogEntry ll b:)

  let envAws1 = envAws0 { AWS.logger = logger }

  (fromEither =<< embed (liftIO $ runResourceT $ AWS.sendEither envAws1 req))
    & do flip finally do
            entries <- L.reverse <$> readTVarIO tStack
            forM_ entries dataLog

interpretDataLogAwsLogEntryToLog :: forall r. ()
  => Member Log r
  => InterpreterFor (DataLog AwsLogEntry) r
interpretDataLogAwsLogEntryToLog =
  interpretDataLogAwsLogEntryToLogWith awsLogLevelToSeverity

interpretDataLogAwsLogEntryToLogWith :: forall r. ()
  => Member Log r
  => (AWS.LogLevel -> Severity)
  -> InterpreterFor (DataLog AwsLogEntry) r
interpretDataLogAwsLogEntryToLogWith mapSeverity
  = interpretDataLogAwsLogEntryLocalToLogWith mapSeverity id

interpretDataLogAwsLogEntryLocalToLogWith :: forall r. ()
  => Member Log r
  => (AWS.LogLevel -> Severity)
  -> (AwsLogEntry -> AwsLogEntry)
  -> InterpreterFor (DataLog AwsLogEntry) r
interpretDataLogAwsLogEntryLocalToLogWith mapSeverity context =
  interpretH \case
    Log.DataLog logEntry -> do
      let severity = mapSeverity (logEntry ^. the @"logLevel")
      let text = LT.toStrict (LT.decodeUtf8 (B.toLazyByteString (logEntry ^. the @"builder")))
      liftT (log severity text)
    Log.Local f ma ->
      raise . interpretDataLogAwsLogEntryLocalToLogWith mapSeverity (f . context) =<< runT ma
{-# inline interpretDataLogAwsLogEntryLocalToLogWith #-}

awsLogLevelToSeverity :: ()
  => AWS.LogLevel
  -> Severity
awsLogLevelToSeverity = \case
  AWS.Trace -> Debug
  AWS.Debug -> Debug
  AWS.Info  -> Info
  AWS.Error -> Error
{-# inline awsLogLevelToSeverity #-}
