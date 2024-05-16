module HaskellWorks.Polysemy.Error.Types
  ( GenericError(..)
  , TimedOut(..)
  ) where

import           HaskellWorks.Polysemy.Prelude

newtype GenericError = GenericError
  { message :: Text
  }
  deriving (Eq, Show)

data TimedOut = TimedOut
  deriving (Generic, Eq, Show)
