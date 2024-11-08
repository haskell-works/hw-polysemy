module HaskellWorks.Polysemy.Hedgehog.Time (
    genPOSIXTime,
) where

import           Data.Time.Clock.POSIX (POSIXTime)

import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import           HaskellWorks.Prelude

genPOSIXTime :: Gen POSIXTime
genPOSIXTime = do
    -- Generate a random integer within a reasonable range for POSIX time
    -- POSIXTime is a type synonym for NominalDiffTime, which is in seconds
    -- We'll use a range from 0 to a large number of seconds to cover a wide time span
    seconds <- Gen.integral (Range.linear 0 4_102_444_800) -- Up to year 2100
    return $ fromIntegral @Word64 seconds
