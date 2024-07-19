module HaskellWorks.Polysemy.Control.Concurrent.STM.TVar
  ( TVar,
    STM.newTVar,
    newTVarIO,
    STM.readTVar,
    readTVarIO,
    STM.writeTVar,
    STM.modifyTVar,
    STM.modifyTVar',
    STM.stateTVar,
    STM.swapTVar,
    registerDelay,
  ) where

import           Control.Concurrent.STM        (TVar)

import qualified Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class        (MonadIO (..))
import           HaskellWorks.Polysemy.Prelude
import           Polysemy

newTVarIO :: ()
  => MonadIO m
  => Member (Embed m) r
  => a
  -> Sem r (TVar a)
newTVarIO a = do
  embed $ liftIO $ STM.newTVarIO a

readTVarIO :: ()
  => MonadIO m
  => Member (Embed m) r
  => TVar a
  -> Sem r a
readTVarIO tvar = do
  embed $ liftIO $ STM.readTVarIO tvar

registerDelay :: ()
  => MonadIO m
  => Member (Embed m) r
  => Int
  -> Sem r (TVar Bool)
registerDelay n = do
  embed $ liftIO $ STM.registerDelay n
