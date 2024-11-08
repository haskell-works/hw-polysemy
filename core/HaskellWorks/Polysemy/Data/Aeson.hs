module HaskellWorks.Polysemy.Data.Aeson
  ( aesonDecode,
  ) where

import qualified Data.Aeson                                        as Aeson
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy        as LBS
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

import           Data.Aeson                                        (FromJSON)
import           HaskellWorks.Polysemy.Error.Types.JsonDecodeError

aesonDecode :: forall a r. ()
  => Member (Error JsonDecodeError) r
  => FromJSON a
  => LBS.ByteString
  -> Sem r a
aesonDecode bs =
  fromEither (Aeson.eitherDecode bs)
    & mapError newJsonDecodeError
