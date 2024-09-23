{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use let" -}

module HaskellWorks.Polysemy.Log
  ( interpretDataLogNoop,
    interpretDataLogLocalNoop,
    interpretDataLogToJsonStdout,
    logEntryToJson,
    logMessageToJson,

    annotateCs,
    logCs,
  ) where

import           Data.Aeson
import qualified Data.Aeson                  as J
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text.Encoding          as T
import           Data.Time
import qualified GHC.Stack                   as GHC
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Internal.Tactics   (liftT)
import           Polysemy.Log
import qualified Polysemy.Log.Effect.DataLog as Log
import           Polysemy.Time
import qualified Polysemy.Time               as Time

interpretDataLogNoop :: forall a r. ()
  => InterpreterFor (DataLog a) r
interpretDataLogNoop =
  interpretDataLogLocalNoop id

interpretDataLogLocalNoop :: forall a r. ()
  => (a -> a)
  -> InterpreterFor (DataLog a) r
interpretDataLogLocalNoop context =
  interpretH \case
    Log.DataLog _ ->
      liftT (pure ())
    Log.Local f ma ->
      raise . interpretDataLogLocalNoop (f . context) =<< runT ma
{-# inline interpretDataLogLocalNoop #-}

interpretDataLogToJsonStdout :: forall e a r. ()
  => Member (Embed IO) r
  => (e -> J.Value)
  -> Sem (DataLog e : r) a
  -> Sem r a
interpretDataLogToJsonStdout toJson =
  interpretDataLogStdoutWith (T.decodeUtf8 . LBS.toStrict . J.encode . toJson)

-- | Log a datalog message with the given severity and provided callstack.
annotateCs :: forall a r. ()
  => Member GhcTime r
  => CallStack
  -> a
  -> Sem r (LogEntry a)
annotateCs cs msg = do
  time <- Time.now @UTCTime @Day
  pure (LogEntry msg time cs)

-- | Log a text message with the given severity and provided callstack.
logCs :: ()
  => Members [Logger, GhcTime] r
  => CallStack
  -> Severity
  -> Text
  -> Sem r ()
logCs cs severity message =
  withFrozenCallStack do
    send . DataLog =<< annotateCs cs (LogMessage severity message)
{-# inline logCs #-}

logEntryToJson :: forall a. ()
  => (a -> Value)
  -> LogEntry a
  -> Value
logEntryToJson aToJson (LogEntry value time callstack) =
    object
      [ "time" .= time
      , "data" .= aToJson value
      , "callstack" .= fmap callsiteToJson (GHC.getCallStack callstack)
      ]
    where
      callsiteToJson :: ([Char], GHC.SrcLoc) -> Value
      callsiteToJson (caller, srcLoc) =
        object
          [ "caller"    .= caller
          , "package"   .= GHC.srcLocPackage srcLoc
          , "module"    .= GHC.srcLocModule srcLoc
          , "file"      .= GHC.srcLocFile srcLoc
          , "startLine" .= GHC.srcLocStartLine srcLoc
          , "startCol"  .= GHC.srcLocStartCol srcLoc
          , "endLine"   .= GHC.srcLocEndLine srcLoc
          , "endCol"    .= GHC.srcLocEndCol srcLoc
          ]

logMessageToJson :: LogMessage -> Value
logMessageToJson (LogMessage severity message) =
    object
      [ "severity" .= show severity
      , "message"  .= message
      ]
