{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}

module HaskellWorks.TestContainers.LocalStack.Types
  ( LocalStackEndpoint(..)
  ) where

import           HaskellWorks.Prelude

data LocalStackEndpoint = LocalStackEndpoint
  { host :: String
  , port :: Int
  } deriving (Eq, Show, Generic)
