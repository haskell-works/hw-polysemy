module HaskellWorks.Polysemy.Data.Either
  ( onLeftThrow
  ) where

import           HaskellWorks.Polysemy.Prelude

import           Polysemy
import           Polysemy.Error

onLeftThrow :: forall e a r. ()
  => Member (Error e) r
  => Sem r (Either e a)
  -> Sem r a
onLeftThrow f =
  f >>= either throw pure
