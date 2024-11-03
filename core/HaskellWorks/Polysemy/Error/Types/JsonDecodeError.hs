{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Polysemy.Error.Types.JsonDecodeError
  ( JsonDecodeError(..)
  ) where

import           HaskellWorks.Prelude

newtype JsonDecodeError =
  JsonDecodeError
  { message :: String
  }
  deriving (Eq, Generic, Show)
