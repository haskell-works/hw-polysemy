module HaskellWorks.Polysemy.Control.Concurrent.STM
  ( TVar,
    atomically,
    STM.orElse,
    STM.retry,
    STM.check,
    STM.throwSTM,
    STM.catchSTM,
  ) where

import           Control.Concurrent.STM        (STM, TVar)
import qualified Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class        (MonadIO (..))
import           HaskellWorks.Polysemy.Prelude
import           Polysemy

atomically :: forall a m r. ()
  => MonadIO m
  => Member (Embed m) r
  => STM a
  -> Sem r a
atomically m =
  embed $ liftIO $ STM.atomically m
