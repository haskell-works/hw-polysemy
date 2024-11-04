module HaskellWorks.Polysemy.Hedgehog.Property
  ( Property,
    property,
    propertyOnce,
  ) where

import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import           Hedgehog                                       (Property)
import qualified Hedgehog                                       as H

import           Control.Monad.IO.Class                         (liftIO)
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Effect.Log
import           HaskellWorks.Polysemy.Log
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Log
import           Polysemy.Resource
import           Polysemy.Time.Interpreter.Ghc

property :: ()
  => Sem
        [ Log
        , DataLog (LogEntry LogMessage)
        , DataLog Text
        , GhcTime
        , Hedgehog
        , Embed IO
        , Embed (H.PropertyT IO)
        , Resource
        , Final (H.PropertyT IO)
        ] ()
  -> H.Property
property f =
    f & interpretLogDataLog
      & setLogLevelFromEnv "LOG_LEVEL" Info
      & interpretDataLogHedgehog formatLogEntry getLogEntryCallStack
      & interpretDataLogHedgehog id (const GHC.callStack)
      & interpretTimeGhc
      & hedgehogToPropertyFinal
      & runEmbedded liftIO
      & embedToFinal @(H.PropertyT IO)
      & runResource
      & runFinal
      & H.property
      & H.withTests 1

propertyOnce :: ()
  => Sem
        [ Log
        , DataLog (LogEntry LogMessage)
        , DataLog Text
        , GhcTime
        , Hedgehog
        , Embed IO
        , Embed (H.PropertyT IO)
        , Resource
        , Final (H.PropertyT IO)
        ] ()
  -> H.Property
propertyOnce f =
    f & property
      & H.withTests 1
