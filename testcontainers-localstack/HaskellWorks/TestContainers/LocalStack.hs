{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use camelCase" -}

module HaskellWorks.TestContainers.LocalStack
  ( LocalStackEndpoint(..)
  , TC.Container
  , setupContainers
  , setupContainers'
  , waitForLocalStack
  ) where

import           Prelude

import           Control.Concurrent                           (threadDelay)
import           Control.Exception                            (try)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy                         as LBS
import           Data.Function
import qualified Data.Text                                    as T
import           Data.Time.Clock.POSIX                        (getPOSIXTime)
import           HaskellWorks.Prelude
import           HaskellWorks.TestContainers.LocalStack.Types (LocalStackEndpoint (LocalStackEndpoint))
import           Network.HTTP.Conduit                         (HttpException,
                                                               simpleHttp)
import qualified System.Environment                           as IO
import qualified TestContainers.Monad                         as TC
import qualified TestContainers.Tasty                         as TC

-- | Sets up and runs the containers required for this test suite.
setupContainers :: ()
  => TC.MonadDocker m
  => m TC.Container
setupContainers = setupContainers' "localstack/localstack-pro:latest"

-- | Sets up and runs the containers required for this test suite.
setupContainers' :: ()
  => TC.MonadDocker m
  => Text
  -> m TC.Container
setupContainers' dockerTag = do
  authToken <- liftIO $ IO.lookupEnv "LOCALSTACK_AUTH_TOKEN"
  -- Launch the container based on the postgres image.
  localstackContainer <- TC.run $ TC.containerRequest (TC.fromTag dockerTag)
    & TC.setEnv [("LOCALSTACK_AUTH_TOKEN", maybe "" T.pack authToken)]
    -- Expose the port 4566 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    & TC.setExpose
        ( mconcat
            [ [ 4566 ]
            ]
        )
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    & TC.setWaitingFor (TC.waitUntilMappedPortReachable 4566)

  -- Look up the corresponding port on the host machine for the exposed port 4566.
  let localStackPort = TC.containerPort localstackContainer 4566

  liftIO $ waitForLocalStack "localhost" localStackPort 8

  pure localstackContainer

waitForLocalStack :: String -> Int -> Int -> IO ()
waitForLocalStack host port timeout = do
    startTime <- getPOSIXTime
    let url = "http://" ++ host ++ ":" ++ show port
    checkLoop startTime url
  where
    checkLoop startTime url = do
        result <- try $ simpleHttp url :: IO (Either HttpException LBS.ByteString)
        case result of
            Right _ -> putStrLn "LocalStack is ready!"
            Left _  -> do
                currentTime <- getPOSIXTime
                let elapsedTime = currentTime - startTime
                when (elapsedTime < fromIntegral timeout) $ do
                    threadDelay 1000000  -- Wait for 1 second
                    checkLoop startTime url
                when (elapsedTime >= fromIntegral timeout) $ do
                    putStrLn "Timeout reached. LocalStack is not ready."
