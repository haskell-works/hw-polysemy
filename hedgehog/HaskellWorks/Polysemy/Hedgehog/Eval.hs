module HaskellWorks.Polysemy.Hedgehog.Eval
  ( evalIO_
  , evalM_
  , eval
  , evalM
  , evalIO

  ) where

import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           Polysemy

evalIO_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => IO a
  -> Sem r ()
evalIO_ = void . evalIO

evalM_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r a
  -> Sem r ()
evalM_ = void . evalM
