{- HLINT ignore "Use let" -}

module HaskellWorks.Polysemy.Amazonka.LocalStack
  ( runReaderLocalAwsEnvDiscover,
    getLocalStackEndpoint,
    inspectContainer,
  ) where

import qualified Amazonka                                     as AWS
import qualified Amazonka.Auth                                as AWS
import           Control.Lens                                 ((%~), (.~))
import qualified Data.Aeson                                   as J
import           Data.Generics.Product.Any
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import qualified HaskellWorks.TestContainers.LocalStack.Types as Z
import           Polysemy
import           Polysemy.Reader
import qualified System.IO                                    as IO
import qualified TestContainers.Monad                         as TC
import qualified TestContainers.Tasty                         as TC

runReaderLocalAwsEnvDiscover :: forall a r. ()
  => Member (Embed IO) r
  => IO TC.Container
  -> Sem (Reader AWS.Env : r) a
  -> Sem r a
runReaderLocalAwsEnvDiscover mk f = do
  container <- embed mk
  ep <- getLocalStackEndpoint container

  logger' <- embed $ AWS.newLogger AWS.Debug IO.stdout

  let creds = AWS.fromKeys (AWS.AccessKey "test") (AWS.SecretKey "test")

  credEnv <- embed $ AWS.newEnv (AWS.runCredentialChain [pure . creds])

  awsEnv <- pure $
    credEnv
      & the @"logger" .~ logger'
      & the @"overrides" %~ (. AWS.setEndpoint False "localhost" ep.port)

  runReader awsEnv f

getLocalStackEndpoint :: ()
  => TC.Container
  -> Sem r LocalStackEndpoint
getLocalStackEndpoint container = do
  let localStackPort = TC.containerPort container 4566

  pure Z.LocalStackEndpoint
    { Z.host = "0.0.0.0"
    , Z.port = localStackPort
    }

inspectContainer :: ()
  => Member (Embed IO) r
  => TC.Container
  -> Sem r J.Value
inspectContainer container =
  embed $ TC.runTestContainer TC.defaultDockerConfig $ TC.inspect container
