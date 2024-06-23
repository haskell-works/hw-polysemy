module HaskellWorks.Polysemy.Data.Aeson
  ( AesonDecodeError(..)
  , aesonDecode
  ) where

import qualified Data.Aeson                                 as Aeson
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy as LBS
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

import           Data.Aeson                                 (FromJSON)

newtype AesonDecodeError
  = AesonDecodeError String
  deriving (Show)

aesonDecode :: ()
  => Member (Error AesonDecodeError) r
  => FromJSON a
  => LBS.ByteString
  -> Sem r a
aesonDecode bs =
  fromEither (Aeson.eitherDecode bs)
    & mapError AesonDecodeError
