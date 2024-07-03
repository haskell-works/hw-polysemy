{-# LANGUAGE GADTs #-}

-- | Description: The 'ResourceT' effect
module HaskellWorks.Polysemy.Managed
  ( Managed,
    ManagedResource(..),
    runManagedToFinal,
    runManagedToFinalIO,
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal
import           Data.IORef                            (IORef)
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Reader
import           Polysemy.Resource

newtype ManagedResource =
  ManagedResource
    { unManagedResource :: IORef ReleaseMap
    }

type Managed = Reader ManagedResource

runManagedToFinal :: ()
  => MonadIO m
  => Member Resource r
  => Member (Final m) r
  => Sem (Managed ': r) a
  -> Sem r a
runManagedToFinal f = do
  istate <- embedFinal $ liftIO createInternalState

  runReader (ManagedResource istate) f
    & flip finally do
        embedFinal $ liftIO $ stateCleanupChecked Nothing istate

runManagedToFinalIO :: ()
  => Member Resource r
  => Member (Final IO) r
  => Sem (Managed ': r) a
  -> Sem r a
runManagedToFinalIO = runManagedToFinal
