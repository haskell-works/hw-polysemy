{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use let" -}

module HaskellWorks.Polysemy.TestContainers.LocalStackSpec
  ( tasty_local_stack
  ) where

import           Prelude

import           HaskellWorks.TestContainers.LocalStack
import           Test.Polysemy.Env
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.Hedgehog                       as H

import qualified Amazonka.S3                               as AWS
import           HaskellWorks.Polysemy.Amazonka
import           HaskellWorks.Polysemy.Amazonka.LocalStack
import           HaskellWorks.Polysemy.Hedgehog
import           HaskellWorks.Prelude
import           Polysemy
import qualified TestContainers.Tasty                      as TC

{- HLINT ignore "Use camelCase" -}

tasty_local_stack :: Tasty.TestTree
tasty_local_stack =
  TC.withContainers setupContainers $ \getContainer ->
    H.testProperty "Local stack test" $ propertyOnce $ runLocalTestEnv getContainer $ do
      container <- embed getContainer
      ep <- getLocalStackEndpoint container
      jotYamlM_ $ inspectContainer container
      jotShow_ ep
      listBucketsReq <- pure AWS.newListBuckets

      listBucketResp <- sendAws listBucketsReq
        & trapFail

      jotShow_ listBucketResp
