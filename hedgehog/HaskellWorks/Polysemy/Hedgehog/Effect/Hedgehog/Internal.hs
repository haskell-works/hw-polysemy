{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog.Internal
  ( MonadAssertion(..),
    failWithCustom,
  ) where

import           HaskellWorks.Polysemy.Prelude
import qualified Hedgehog                              as H
import qualified Hedgehog.Internal.Property            as H
import qualified Hedgehog.Internal.Source              as H

import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except            as E
import qualified Control.Monad.Trans.Resource          as IO
import qualified Control.Monad.Trans.Resource.Internal as IO

failWithCustom :: ()
  => H.MonadTest m
  => CallStack
  -> Maybe H.Diff
  -> String
  -> m a
failWithCustom cs mdiff msg =
  H.liftTest $ H.mkTest (Left $ H.Failure (H.getCaller cs) msg mdiff, mempty)
class Monad m => MonadAssertion m where
  throwAssertion :: H.Failure -> m a
  catchAssertion :: m a -> (H.Failure -> m a) -> m a

instance Monad m => MonadAssertion (H.TestT m) where
  throwAssertion f = H.liftTest $ H.mkTest (Left f, mempty)
  catchAssertion g h = H.TestT $ E.catchE (H.unTest g) (H.unTest . h)

instance MonadAssertion m => MonadAssertion (IO.ResourceT m) where
  throwAssertion = lift . throwAssertion
  catchAssertion r h = IO.ResourceT $ \i -> IO.unResourceT r i `catchAssertion` \e -> IO.unResourceT (h e) i

deriving instance Monad m => MonadAssertion (H.PropertyT m)
