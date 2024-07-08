{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Polysemy.Amazonka
  ( sendAws
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Typeable

import qualified Amazonka                     as AWS
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

sendAws :: ()
  => AWS.AWSRequest a
  => Member (Embed m) r
  => Member (Error AWS.Error) r
  => Member (Reader AWS.Env) r
  => MonadIO m
  => Typeable (AWS.AWSResponse a)
  => Typeable a
  => a
  -> Sem r (AWS.AWSResponse a)
sendAws req = do
  envAws <- ask @AWS.Env
  result <- embed $ liftIO $ runResourceT $ AWS.sendEither envAws req
  fromEither result
