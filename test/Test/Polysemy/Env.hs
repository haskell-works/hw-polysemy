{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Use let" -}

module Test.Polysemy.Env
  ( EnvironmentVariableMissing(..)
  , runLocalTestEnv
  , runTestEnv
  , runReaderFromEnvOrFail
  ) where

import qualified Amazonka                                 as AWS
import qualified Amazonka.Auth                            as AWS
import           Control.Lens                             ((%~), (.~), (^.))
import           Data.Generics.Product.Any
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import qualified System.IO                                as IO

newtype EnvironmentVariableMissing =
  EnvironmentVariableMissing String
  deriving (Show, Eq)

runTestEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Sem
        ( Reader AWS.Env
        : r)
      a
  -> Sem r a
runTestEnv f =
  withFrozenCallStack $ f
    & runReaderAwsEnvDiscover

runLocalTestEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => IO LocalStackEndpoint
  -> Sem
        ( Reader AWS.Env
        : r)
      a
  -> Sem r a
runLocalTestEnv mk f =
  withFrozenCallStack $ f
    & runReaderLocalAwsEnvDiscover mk

runReaderFromEnvOrFail :: forall i r a. ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => (String -> i)
  -> String
  -> Sem (Reader i ': r) a
  -> Sem r a
runReaderFromEnvOrFail f envVar action = do
  env <- lookupEnv envVar
    & onNothingM (throw (EnvironmentVariableMissing envVar) & trapFail)

  runReader (f env) action

runReaderLocalAwsEnvDiscover :: ()
  => Member (Embed IO) r
  => IO LocalStackEndpoint
  -> Sem (Reader AWS.Env : r) a
  -> Sem r a
runReaderLocalAwsEnvDiscover mk f = do
  ep <- embed mk

  logger' <- embed $ AWS.newLogger AWS.Debug IO.stdout

  let creds = AWS.fromKeys (AWS.AccessKey "test") (AWS.SecretKey "test")

  credEnv <- embed $ AWS.newEnv (AWS.runCredentialChain [pure . creds])

  awsEnv <- pure $
    credEnv
      & the @"logger" .~ logger'
      & the @"overrides" %~ (. AWS.setEndpoint False "localhost" (ep ^. the @"port"))

  runReader awsEnv f

