{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Polysemy.Error.Types.YamlDecodeError
  ( YamlDecodeError(..)
  ) where


import           Data.Aeson                (ToJSON (..), (.=))
import qualified Data.Aeson                as J
import           GHC.Generics

import           Data.Generics.Product.Any
import           HaskellWorks.Prelude
import           Lens.Micro
newtype YamlDecodeError =
  YamlDecodeError
  { message :: String
  }
  deriving (Eq, Generic, Show)

instance ToJSON YamlDecodeError where
  toJSON e =
        J.object
            [ "error" .= id @Text "YamlDecodeError"
            , "message" .= (e ^. the @"message")
            ]
