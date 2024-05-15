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
import           HaskellWorks.Polysemy.Prelude
import           Polysemy

newTVarIO :: ()
  => Member (Embed IO) r
  => a
  -> Sem r (TVar a)
newTVarIO a = do
  embed $ STM.newTVarIO a

readTVarIO :: ()
  => Member (Embed IO) r
  => TVar a
  -> Sem r a
readTVarIO tvar = do
  embed $ STM.readTVarIO tvar

registerDelay :: ()
  => Member (Embed IO) r
  => Int
  -> Sem r (TVar Bool)
registerDelay n = do
  embed $ STM.registerDelay n
