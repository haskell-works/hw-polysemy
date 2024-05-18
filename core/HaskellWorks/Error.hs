module HaskellWorks.Error
  ( onLeft
  , onNothing
  , onLeftM
  , onNothingM
  , onFalse
  , onTrue
  , onFalseM
  , onTrueM
  , onFalse_
  , onTrue_
  , onFalseM_
  , onTrueM_
  ) where

import           HaskellWorks.Polysemy.Prelude

onLeft :: forall e m a. Monad m => (e -> m a) -> Either e a -> m a
onLeft h = either h pure

onFalse :: Monad m => m Bool -> Bool -> m Bool
onFalse h = bool h (pure True)

onTrue :: Monad m => m Bool -> Bool -> m Bool
onTrue h = bool (pure False) h

onFalseM :: Monad m => m Bool -> m Bool -> m Bool
onFalseM h f = f >>= onFalse h

onTrueM :: Monad m => m Bool -> m Bool -> m Bool
onTrueM h f = f >>= onTrue h

onFalse_ :: Monad m => m () -> Bool -> m ()
onFalse_ h = bool h (pure ())

onTrue_ :: Monad m => m () -> Bool -> m ()
onTrue_ h = bool (pure ()) h

onFalseM_ :: Monad m => m () -> m Bool -> m ()
onFalseM_ h f = f >>= onFalse_ h

onTrueM_ :: Monad m => m () -> m Bool -> m ()
onTrueM_ h f = f >>= onTrue_ h

onNothing :: forall a m. Monad m => m a -> Maybe a -> m a
onNothing h = maybe h return

onLeftM :: forall e m a. Monad m => (e -> m a) -> m (Either e a) -> m a
onLeftM h action = onLeft h =<< action

onNothingM :: forall a m. Monad m => m a -> m (Maybe a) -> m a
onNothingM h f = onNothing h =<< f
