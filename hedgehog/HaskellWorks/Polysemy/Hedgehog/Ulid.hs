module HaskellWorks.Polysemy.Hedgehog.Ulid (
    genUlid,
    genUlidRandom,
    genUlidTimeStamp,
) where

import           Data.Binary                         (decodeOrFail)
import qualified Data.ByteString.Lazy                as LBS
import           Data.ULID                           (ULID (..))
import           Data.ULID.Random                    (ULIDRandom)
import           Data.ULID.TimeStamp                 (ULIDTimeStamp,
                                                      mkULIDTimeStamp)
import           HaskellWorks.Polysemy.Hedgehog.Time

import           Hedgehog
import qualified Hedgehog.Gen                        as Gen
import qualified Hedgehog.Range                      as Range

import           HaskellWorks.Prelude

genUlidRandom :: Gen ULIDRandom
genUlidRandom = do
    bytes <- Gen.bytes (Range.singleton 10) -- 80 bits
    let lazyBytes = LBS.fromStrict bytes
    case decodeOrFail lazyBytes of
        Left (_, _, err)   -> fail $ "Failed to decode ULIDRandom: " <> err -- This shouldn't happen.
        Right (_, _, ulid) -> pure ulid

genUlidTimeStamp :: Gen ULIDTimeStamp
genUlidTimeStamp =
    mkULIDTimeStamp <$> genPOSIXTime

genUlid :: Gen ULID
genUlid =
    ULID <$> genUlidTimeStamp <*> genUlidRandom
