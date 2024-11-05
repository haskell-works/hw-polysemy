{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Polysemy.Error.Types.JsonDecodeError
  ( JsonDecodeError(..)
  ) where


import           Data.Aeson                (ToJSON (..), (.=))
import qualified Data.Aeson                as J
import           GHC.Generics

import           Data.Generics.Product.Any
import           HaskellWorks.Prelude
import           Lens.Micro

newtype JsonDecodeError =
  JsonDecodeError
  { message :: String
  }
  deriving (Eq, Generic, Show)

instance ToJSON JsonDecodeError where
  toJSON e =
        J.object
            [ "error" .= id @Text "JsonDecodeError"
            , "message" .= (e ^. the @"message")
            ]
