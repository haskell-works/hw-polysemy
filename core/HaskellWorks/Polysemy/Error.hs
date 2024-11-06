module HaskellWorks.Polysemy.Error
  ( module HaskellWorks.Error,
    trap,
    trap_,
    runErrorMap,
    embedRunExceptT,
    embedThrowExceptT,
  ) where

import           Control.Monad.Except
import           HaskellWorks.Error
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

-- | Run a computation that may fail, and handle the error case.
-- Unlike 'catch' from 'Polysemy.Error' this function removes the 'Error'
-- effect from the stack.
trap :: forall e a r. ()
  => (e -> Sem r a)
  -> Sem (Error e ': r) a
  -> Sem r a
trap h f =
  runError f >>= either h pure

-- | Like 'trap', but the error is not passed to the handler.
trap_ :: forall e a r. ()
  => Sem r a
  -> Sem (Error e ': r) a
  -> Sem r a
trap_ h =
  trap (const h)

-- | Run an 'Error' effect and map the error value to a different type.
runErrorMap :: ()
  => (e -> d)
  -> Sem (Error e : r) a
  -> Sem r (Either d a)
runErrorMap f =
  fmap (first f) . runError

embedRunExceptT :: forall e a r m. ()
  => Member (Embed m) r
  => ExceptT e m a
  -> Sem r (Either e a)
embedRunExceptT = embed . runExceptT

-- | Run an embedded 'ExceptT' effect in a 'Sem' monad and throw any errors.
embedThrowExceptT :: forall e a r m. ()
  => Member (Error e) r
  => Member (Embed m) r
  => ExceptT e m a
  -> Sem r a
embedThrowExceptT f =
  embedRunExceptT f
    & onLeftM throw
