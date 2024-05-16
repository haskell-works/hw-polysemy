{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
  ( Hedgehog

  , assertEquals
  , eval
  , evalM
  , evalIO
  , writeLog
  , failWith
  , failWithCustom

  , hedgehogToIntegrationFinal

  ) where

import qualified GHC.Stack                                               as GHC
import           HaskellWorks.Polysemy.Prelude

import qualified Hedgehog                                                as H
import qualified Hedgehog.Internal.Property                              as H

import qualified HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog.Internal as I
import           Polysemy
import           Polysemy.Final

data Hedgehog m rv where
  AssertEquals :: (GHC.HasCallStack, Eq a, Show a)
    => a
    -> a
    -> Hedgehog m ()

  Eval :: GHC.HasCallStack
    => a
    -> Hedgehog m a

  EvalM :: GHC.HasCallStack
    => m a
    -> Hedgehog m a

  EvalIO :: GHC.HasCallStack
    => IO a
    -> Hedgehog m a

  WriteLog :: ()
    => H.Log
    -> Hedgehog m ()

  FailWith :: GHC.HasCallStack
    => Maybe H.Diff
    -> String
    -> Hedgehog m a

  FailWithCustom :: ()
    => GHC.CallStack
    -> Maybe H.Diff
    -> String
    -> Hedgehog m a

makeSem ''Hedgehog

hedgehogToIntegrationFinal :: ()
  => Member (Final (H.PropertyT IO)) r
  => Sem (Hedgehog ': r) a
  -> Sem r a
hedgehogToIntegrationFinal = interpretFinal \case
  AssertEquals a b ->
    liftS $ a H.=== b
  Eval a ->
    liftS $ H.eval a
  EvalIO f ->
    liftS $ H.evalIO f
  EvalM f -> do
    g <- runS f
    pure $ H.evalM g
  FailWith mdiff msg ->
    liftS $ H.failWith mdiff msg
  FailWithCustom cs mdiff msg ->
    liftS $ I.failWithCustom cs mdiff msg
  WriteLog logValue ->
    liftS $ H.writeLog logValue