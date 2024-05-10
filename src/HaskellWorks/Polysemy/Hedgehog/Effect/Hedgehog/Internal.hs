module HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog.Internal
  ( failWithCustom
  ) where

import           HaskellWorks.Polysemy.Prelude
import qualified Hedgehog                      as H
import qualified Hedgehog.Internal.Property    as H
import qualified Hedgehog.Internal.Source      as H

failWithCustom :: ()
  => H.MonadTest m
  => CallStack
  -> Maybe H.Diff
  -> String
  -> m a
failWithCustom cs mdiff msg =
  H.liftTest $ H.mkTest (Left $ H.Failure (H.getCaller cs) msg mdiff, mempty)
