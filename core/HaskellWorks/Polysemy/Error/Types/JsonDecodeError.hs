{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module HaskellWorks.Polysemy.Error.Types.JsonDecodeError
  ( JsonDecodeError(..)
  , newJsonDecodeError
  ) where


import           Data.Aeson           (ToJSON (..), Value, (.=))
import qualified Data.Aeson           as J
import           GHC.Generics

import           HaskellWorks.Prelude
import           HaskellWorks.ToText

data JsonDecodeError =
  JsonDecodeError
  { message    :: Text
  , bytestring :: Maybe ByteString
  , text       :: Maybe Text
  , json       :: Maybe Value
  }
  deriving (Eq, Generic, Show)

newJsonDecodeError :: ToText a => a -> JsonDecodeError
newJsonDecodeError message =
  JsonDecodeError
    { message    = toText message
    , bytestring = Nothing
    , text       = Nothing
    , json       = Nothing
    }

instance ToJSON JsonDecodeError where
  toJSON e =
    J.object
        [ "error" .= id @Text "JsonDecodeError"
        , "message" .= e.message
        , "text" .= e.text
        , "json" .= e.json
        ]
