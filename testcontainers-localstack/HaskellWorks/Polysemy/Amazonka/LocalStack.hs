{- HLINT ignore "Use let" -}

module HaskellWorks.Polysemy.Amazonka.LocalStack
  ( runReaderLocalAwsEnvDiscover
  ) where

import qualified Amazonka                               as AWS
import qualified Amazonka.Auth                          as AWS
import           Control.Lens                           ((%~), (.~), (^.))
import           Data.Generics.Product.Any
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack
import           Polysemy
import           Polysemy.Reader
import qualified System.IO                              as IO

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
