module HaskellWorks.Polysemy.Hedgehog.Test
  ( Property,
    test,

  ) where

import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import           Hedgehog                                       (Property)
import qualified Hedgehog                                       as H

import           Control.Monad.IO.Class                         (liftIO)
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Effect.Log
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Log
import           Polysemy.Resource
import           Polysemy.Time.Interpreter.Ghc

test :: ()
  => Sem
        [ Log
        , DataLog (LogEntry LogMessage)
        , DataLog Text
        , GhcTime
        , Hedgehog
        , Embed IO
        , Embed (H.TestT IO)
        , Resource
        , Final (H.TestT IO)
        ] ()
  -> H.Property
test f = f
  & interpretLogDataLog
  & setLogLevel (Just Info)
  & interpretDataLogHedgehog formatLogEntry getLogEntryCallStack
  & interpretDataLogHedgehog id (const GHC.callStack)
  & interpretTimeGhc
  & hedgehogToTestFinal
  & runEmbedded liftIO
  & embedToFinal @(H.TestT IO)
  & runResource
  & runFinal
  & H.test
  & H.property
  & H.withTests 1
