{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Use let" -}

module Test.Polysemy.Env
  ( EnvironmentVariableMissing(..)
  , runLocalTestEnv
  , runTestEnv
  , runReaderFromEnvOrFail
  ) where

import qualified Amazonka                                  as AWS
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Polysemy.System.Environment
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import qualified TestContainers.Tasty                      as TC

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
  => IO TC.Container
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
