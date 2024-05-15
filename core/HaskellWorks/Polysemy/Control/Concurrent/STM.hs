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
import           HaskellWorks.Polysemy.Prelude
import           Polysemy

atomically :: ()
  => Member (Embed IO) r
  => STM a
  -> Sem r a
atomically m =
  embed $ STM.atomically m
