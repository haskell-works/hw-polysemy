{-# LANGUAGE GADTs #-}

-- | Description: The 'Except' effect, providing catch and throw functionality over the final
-- monad via MonadCatch.
module HaskellWorks.Polysemy.Except
  ( catchEx,
    throwEx,
    mask,
    maskM,
    uninterruptibleMask,
    uninterruptibleMaskM,
  ) where

import           Control.Exception    (Exception (..))
import           Control.Monad.Catch  (MonadCatch, MonadMask, MonadThrow)
import qualified Control.Monad.Catch  as X
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Final

catchEx :: ()
  => MonadCatch m
  => Member (Final m) r
  => Exception e
  => Sem r a
  -> (e -> Sem r a)
  -> Sem r a
catchEx f h =
  withWeavingToFinal \s lower _ ->
    X.catch (lower (f <$ s)) \e -> lower (h e <$ s)

throwEx :: ()
  => MonadThrow m
  => Member (Final m) r
  => Exception e
  => e
  -> Sem r a
throwEx e =
  embedFinal $ X.throwM e

maskM :: ()
  => MonadMask m
  => Member (Final m) r
  => ((forall . m b -> m b) -> Sem r a)
  -> Sem r a
maskM f =
  withWeavingToFinal \s lower _ ->
    X.mask \restore ->
      lower (f restore <$ s)

mask :: ()
  => MonadMask m
  => Member (Final m) r
  => ((forall b. Sem r b -> Sem r b) -> Sem r a)
  -> Sem r a
mask f =
  withWeavingToFinal \s lower _ ->
    X.mask \restore ->
      lower $ f (\a ->
        withWeavingToFinal \s2 lower2 _ ->
          restore $ lower2 (a <$ s2)
      ) <$ s

uninterruptibleMaskM :: ()
  => MonadMask m
  => Member (Final m) r
  => ((forall . m b -> m b) -> Sem r a)
  -> Sem r a
uninterruptibleMaskM f =
  withWeavingToFinal \s lower _ ->
    X.uninterruptibleMask \restore ->
      lower (f restore <$ s)

uninterruptibleMask :: ()
  => MonadMask m
  => Member (Final m) r
  => ((forall b. Sem r b -> Sem r b) -> Sem r a)
  -> Sem r a
uninterruptibleMask f =
  withWeavingToFinal \s lower _ ->
    X.uninterruptibleMask \restore ->
      lower $ f (\a ->
        withWeavingToFinal \s2 lower2 _ ->
          restore $ lower2 (a <$ s2)
      ) <$ s
