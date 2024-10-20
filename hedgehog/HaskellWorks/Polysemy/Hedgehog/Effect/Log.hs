module HaskellWorks.Polysemy.Hedgehog.Effect.Log
  ( interpretDataLogHedgehog,
    getLogEntryCallStack,
  ) where

import qualified Data.Text                                      as Text
import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           Polysemy
import           Polysemy.Log

interpretDataLogHedgehog :: ()
  => Member Hedgehog r
  => (a -> Text)
  -> (a -> GHC.CallStack)
  -> InterpreterFor (DataLog a) r
interpretDataLogHedgehog fmt cs sem = do
  interpretDataLog (\a -> jotWithCallstack (cs a) $ Text.unpack $ fmt a) sem
{-# inline interpretDataLogHedgehog #-}

getLogEntryCallStack :: LogEntry LogMessage -> GHC.CallStack
getLogEntryCallStack = \case
  LogEntry _ _ cs -> cs
