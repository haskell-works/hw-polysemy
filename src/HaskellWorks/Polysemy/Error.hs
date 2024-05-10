module HaskellWorks.Polysemy.Error
  ( onLeftM
  , onNothingM
  ) where

import           HaskellWorks.Polysemy.Prelude

onLeftM :: Monad m => (e -> m a) -> m (Either e a) -> m a
onLeftM f action = either f pure =<< action

onNothingM :: Monad m => m b -> m (Maybe b) -> m b
onNothingM h f = f >>= maybe h return
