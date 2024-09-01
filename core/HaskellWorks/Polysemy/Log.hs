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
    logEntryToJson,
    logMessageToJson,
  ) where

import           Data.Aeson
import qualified GHC.Stack                   as GHC
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Internal.Tactics   (liftT)
import           Polysemy.Log
import qualified Polysemy.Log.Effect.DataLog as Log

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

logEntryToJson :: (a -> Value) -> LogEntry a -> Value
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
