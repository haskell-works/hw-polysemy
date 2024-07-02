{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
  ( Hedgehog

  , assertEquals
  , catchAssertion
  , eval
  , evalM
  , evalIO
  , writeLog
  , failWith
  , failWithCustom
  , throwAssertion
  , trapAssertion
  , catchEx

  , hedgehogToMonadTestFinal
  , hedgehogToPropertyFinal
  , hedgehogToTestFinal

  ) where

import           HaskellWorks.Polysemy.Prelude

import qualified Hedgehog                                                as H
import qualified Hedgehog.Internal.Property                              as H

import qualified Control.Monad.Catch                                     as IO
import qualified Control.Monad.IO.Class                                  as IO
import           HaskellWorks.Polysemy.Except
import qualified HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog.Internal as I
import           Polysemy
import           Polysemy.Final

data Hedgehog m rv where
  AssertEquals :: (HasCallStack, Eq a, Show a)
    => a
    -> a
    -> Hedgehog m ()

  CatchAssertion :: HasCallStack
    => m a
    -> (H.Failure -> m a)
    -> Hedgehog m a

  Eval :: HasCallStack
    => a
    -> Hedgehog m a

  EvalM :: HasCallStack
    => m a
    -> Hedgehog m a

  EvalIO :: HasCallStack
    => IO a
    -> Hedgehog m a

  FailWith :: HasCallStack
    => Maybe H.Diff
    -> String
    -> Hedgehog m a

  FailWithCustom :: ()
    => CallStack
    -> Maybe H.Diff
    -> String
    -> Hedgehog m a

  ThrowAssertion :: HasCallStack
    => H.Failure
    -> Hedgehog m a

  WriteLog :: HasCallStack
    => H.Log
    -> Hedgehog m ()

makeSem ''Hedgehog

trapAssertion :: ()
  => Member Hedgehog r
  => (H.Failure -> Sem r a)
  -> Sem r a
  -> Sem r a
trapAssertion = flip catchAssertion

hedgehogToMonadTestFinal :: ()
  => IO.MonadIO m
  => IO.MonadCatch m
  => H.MonadTest m
  => I.MonadAssertion m
  => Member (Final m) r
  => Sem (Hedgehog ': r) a
  -> Sem r a
hedgehogToMonadTestFinal = interpretFinal \case
  AssertEquals a b ->
    liftS $ a H.=== b
  CatchAssertion f h -> do
    s  <- getInitialStateS
    f' <- runS f
    h' <- bindS h
    pure $ I.catchAssertion f' $ \e -> do
      h' (e <$ s)
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
    liftS $ I.failWithCustom cs
     mdiff msg
  ThrowAssertion e ->
    liftS $ I.throwAssertion e
  WriteLog logValue ->
    liftS $ H.writeLog logValue

hedgehogToPropertyFinal :: ()
  => Member (Final (H.PropertyT IO)) r
  => Sem (Hedgehog ': r) a
  -> Sem r a
hedgehogToPropertyFinal = hedgehogToMonadTestFinal

hedgehogToTestFinal :: ()
  => Member (Final (H.TestT IO)) r
  => Sem (Hedgehog ': r) a
  -> Sem r a
hedgehogToTestFinal = hedgehogToMonadTestFinal
