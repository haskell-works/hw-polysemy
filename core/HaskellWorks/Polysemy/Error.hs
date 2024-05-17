module HaskellWorks.Polysemy.Error
  ( module HaskellWorks.Error
  , trap
  , trap_
  ) where

import           HaskellWorks.Error
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

-- | Run a computation that may fail, and handle the error case.
-- Unlike 'catch' from 'Polysemy.Error' this function removes the 'Error'
-- effect from the stack.
trap :: forall e r a. ()
  => (e -> Sem r a)
  -> Sem (Error e ': r) a
  -> Sem r a
trap h f =
  runError f >>= either h pure

-- | Like 'trap', but the error is not passed to the handler.
trap_ :: forall e r a. ()
  => Sem r a
  -> Sem (Error e ': r) a
  -> Sem r a
trap_ h =
  trap (const h)
