{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Polysemy.Amazonka
  ( runReaderAwsEnvDiscover,
    sendAws,
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Typeable

import qualified Amazonka                     as AWS
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import qualified System.IO                    as IO

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
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Reader AWS.Env) r
  => MonadIO m
  => Typeable (AWS.AWSResponse a)
  => Typeable a
  => a
  -> Sem r (AWS.AWSResponse a)
sendAws req = do
  envAws <- ask @AWS.Env
  result <- embed $ liftIO $ runResourceT $ AWS.sendEither envAws req
  fromEither result
