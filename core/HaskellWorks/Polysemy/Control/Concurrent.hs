module HaskellWorks.Polysemy.Control.Concurrent
  ( ThreadId,
    myThreadId,
    killThread,
    getNumCapabilities,
    threadCapability,
    yield,
    threadDelay,
    isCurrentThreadBound,
    mkWeakThreadId,
  ) where

import           Control.Concurrent            (ThreadId)
import qualified Control.Concurrent            as IO
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           System.Mem.Weak               (Weak)

myThreadId :: ()
  => Member (Embed IO) r
  => Sem r ThreadId
myThreadId =
  embed IO.myThreadId

killThread :: ()
  => Member (Embed IO) r
  => ThreadId
  -> Sem r ()
killThread tid =
  embed $ IO.killThread tid

getNumCapabilities :: ()
  => Member (Embed IO) r
  => Sem r Int
getNumCapabilities =
  embed IO.getNumCapabilities

threadCapability :: ()
  => Member (Embed IO) r
  => ThreadId
  -> Sem r (Int, Bool)
threadCapability tid =
  embed $ IO.threadCapability tid

yield :: ()
  => Member (Embed IO) r
  => Sem r ()
yield =
  embed IO.yield

threadDelay :: ()
  => Member (Embed IO) r
  => Int
  -> Sem r ()
threadDelay n =
  embed $ IO.threadDelay n

isCurrentThreadBound :: ()
  => Member (Embed IO) r
  => Sem r Bool
isCurrentThreadBound =
  embed IO.isCurrentThreadBound

mkWeakThreadId :: ()
  => Member (Embed IO) r
  => ThreadId
  -> Sem r (Weak ThreadId)
mkWeakThreadId tid =
  embed $ IO.mkWeakThreadId tid
