{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Orphans
  () where

import           Control.Monad.Catch
import           Data.Function
import           Hedgehog
import           Hedgehog.Internal.Property

deriving instance MonadMask m => MonadMask (TestT m)
