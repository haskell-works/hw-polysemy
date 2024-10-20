{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Error.Types
  ( GenericError(..),
    TimedOut(..),
  ) where

import           HaskellWorks.Polysemy.Prelude

newtype GenericError = GenericError
  { message :: Text
  }
  deriving (Generic, Eq, Show)

newtype TimedOut = TimedOut
  { message :: Text
  }
  deriving (Generic, Eq, Show)
