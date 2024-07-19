module HaskellWorks.Polysemy.Control.Concurrent.QSem
  ( QSem,

    newQSem,
    waitQSem,
    signalQSem,
    bracketQSem,
  ) where

import           Control.Concurrent.QSem       (QSem)
import qualified Control.Concurrent.QSem       as IO
import           Control.Monad.IO.Class        (MonadIO (..))
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Resource

newQSem :: ()
  => MonadIO m
  => Member (Embed m) r
  => Int
  -> Sem r QSem
newQSem n = do
  embed $ liftIO $ IO.newQSem n

waitQSem :: ()
  => MonadIO m
  => Member (Embed m) r
  => QSem
  -> Sem r ()
waitQSem sem =
  embed $ liftIO $ IO.waitQSem sem

signalQSem :: ()
  => MonadIO m
  => Member (Embed m) r
  => QSem
  -> Sem r ()
signalQSem sem =
  embed $ liftIO $ IO.signalQSem sem

bracketQSem :: ()
  => MonadIO m
  => Member (Embed m) r
  => Member Resource r
  => QSem
  -> Sem r a
  -> Sem r a
bracketQSem sem =
  bracket_ (waitQSem sem) (signalQSem sem)
