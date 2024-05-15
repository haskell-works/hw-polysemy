module HaskellWorks.Polysemy.Hedgehog.Assert
  ( leftFail
  , leftFailM
  , requireHead
  , catchFail
  , evalIO
  , failure
  , failMessage

  , (===)

  ) where


import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error

(===) :: ()
  => Member Hedgehog r
  => Eq a
  => Show a
  => HasCallStack
  => a
  -> a
  -> Sem r ()
(===) a b = withFrozenCallStack $ assertEquals a b

-- | Fail when the result is Left.
leftFail :: forall e r a. ()
  => Member Hedgehog r
  => Show e
  => HasCallStack
  => Either e a
  -> Sem r a
leftFail r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> failMessage GHC.callStack ("Expected Right: " <> show e)

failure :: ()
  => Member Hedgehog r
  => HasCallStack
  => Sem r a
failure =
  withFrozenCallStack $ failWith Nothing ""

failMessage :: ()
  => Member Hedgehog r
  => HasCallStack
  => GHC.CallStack
  -> String
  -> Sem r a
failMessage cs =
  withFrozenCallStack $ failWithCustom cs Nothing

leftFailM :: forall e r a. ()
  => Member Hedgehog r
  => Show e
  => HasCallStack
  => Sem r (Either e a)
  -> Sem r a
leftFailM f =
  withFrozenCallStack $ f >>= leftFail

catchFail :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => Show e
  => Sem (Error e ': r) a
  -> Sem r a
catchFail f =
  withFrozenCallStack $ f & runError & leftFailM

requireHead :: ()
  => Member Hedgehog r
  => HasCallStack
  => [a]
  -> Sem r a
requireHead = withFrozenCallStack $
  \case
    []    -> failMessage GHC.callStack "Cannot take head of empty list"
    (x:_) -> pure x
