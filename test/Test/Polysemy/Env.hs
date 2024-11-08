{- HLINT ignore "Use let" -}

module Test.Polysemy.Env
  ( EnvironmentVariableMissing(..),
    runLocalTestEnv,
    runTestEnv,
    runReaderFromEnvOrFail,

    interpretDataLogNoop,
    interpretDataLogLocalNoop,
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
import           Polysemy.Internal.Tactics                 (liftT)
import           Polysemy.Log
import qualified Polysemy.Log.Effect.DataLog               as Log
import           Polysemy.Reader
import qualified TestContainers.Tasty                      as TC

newtype EnvironmentVariableMissing =
  EnvironmentVariableMissing String
  deriving (Show, Eq)

runTestEnv :: forall a r. ()
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

runLocalTestEnv :: forall a r. ()
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

runReaderFromEnvOrFail :: forall b a r. ()
  => Member (Embed IO) r
  => Member Hedgehog r
  => (String -> b)
  -> String
  -> Sem (Reader b ': r) a
  -> Sem r a
runReaderFromEnvOrFail f envVar action = do
  env <- lookupEnv envVar
    & onNothingM (throw (EnvironmentVariableMissing envVar) & trapFail)

  runReader (f env) action

interpretDataLogNoop :: forall a r. ()
  => InterpreterFor (DataLog a) r
interpretDataLogNoop =
  interpretDataLogLocalNoop id

interpretDataLogLocalNoop :: forall a r. ()
  => (a -> a)
  -> InterpreterFor (DataLog a) r
interpretDataLogLocalNoop _ =
  interpretH \case
    Log.DataLog _ ->
      liftT (pure ())
    Log.Local f ma ->
      raise . interpretDataLogLocalNoop f =<< runT ma
{-# inline interpretDataLogLocalNoop #-}
