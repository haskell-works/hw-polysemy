module HaskellWorks.Error
  ( onLeft,
    onNothing,
    onLeftM,
    onNothingM,
  ) where

import           HaskellWorks.Polysemy.Prelude

onLeft :: forall e a m. ()
  => Monad m
  => (e -> m a) -> Either e a -> m a
onLeft f = either f pure

onNothing :: forall a m. ()
  => Monad m
  => m a -> Maybe a -> m a
onNothing h = maybe h return

onLeftM :: forall e a m. ()
  => Monad m
  => (e -> m a) -> m (Either e a) -> m a
onLeftM f action = onLeft f =<< action

onNothingM :: forall a m. ()
  => Monad m
  => m a -> m (Maybe a) -> m a
onNothingM h f = onNothing h =<< f
