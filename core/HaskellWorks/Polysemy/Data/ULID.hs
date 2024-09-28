module HaskellWorks.Polysemy.Data.ULID
  ( getULIDTime,
    getULID,
    ulidToInteger,
    ulidFromInteger,
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Time.Clock.POSIX
import qualified Data.ULID              as ULID
import           HaskellWorks.Prelude
import           Polysemy

getULIDTime :: forall r m. ()
  => MonadIO m
  => Member (Embed m) r
  => POSIXTime
  -> Sem r ULID.ULID
getULIDTime time =
  embed $ liftIO $ ULID.getULIDTime time

getULID :: forall r m. ()
  => MonadIO m
  => Member (Embed m) r
  => Sem r ULID.ULID
getULID =
  embed $ liftIO ULID.getULID

ulidToInteger :: ULID.ULID -> Integer
ulidToInteger =
  ULID.ulidToInteger

ulidFromInteger :: Integer -> Either Text ULID.ULID
ulidFromInteger =
  ULID.ulidFromInteger
