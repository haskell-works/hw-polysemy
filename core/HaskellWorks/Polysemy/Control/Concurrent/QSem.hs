module HaskellWorks.Polysemy.Control.Concurrent.QSem
  ( QSem,

    newQSem,
    waitQSem,
    signalQSem,
    bracketQSem,
  ) where

import           Control.Concurrent.QSem       (QSem)
import qualified Control.Concurrent.QSem       as IO
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Resource

newQSem :: ()
  => Member (Embed IO) r
  => Int
  -> Sem r QSem
newQSem n = do
  embed $ IO.newQSem n

waitQSem :: ()
  => Member (Embed IO) r
  => QSem
  -> Sem r ()
waitQSem sem =
  embed $ IO.waitQSem sem

signalQSem :: ()
  => Member (Embed IO) r
  => QSem
  -> Sem r ()
signalQSem sem =
  embed $ IO.signalQSem sem

bracketQSem :: ()
  => Member (Embed IO) r
  => Member Resource r
  => QSem
  -> Sem r a
  -> Sem r a
bracketQSem sem =
  bracket_ (waitQSem sem) (signalQSem sem)
