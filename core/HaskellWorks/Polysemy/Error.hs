module HaskellWorks.Polysemy.Error
  ( onLeft
  , onNothing
  , onLeftM
  , onNothingM
  , trap
  ) where

import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

onLeft :: Monad m => (e -> m a) -> Either e a -> m a
onLeft f = either f pure

onNothing :: Monad m => m b -> Maybe b -> m b
onNothing h = maybe h return

onLeftM :: Monad m => (e -> m a) -> m (Either e a) -> m a
onLeftM f action = onLeft f =<< action

onNothingM :: Monad m => m b -> m (Maybe b) -> m b
onNothingM h f = onNothing h =<< f

trap :: forall e r a. ()
  => (e -> Sem r a)
  -> Sem (Error e ': r) a
  -> Sem r a
trap h f =
  runError f >>= either h pure
