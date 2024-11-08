{-# LANGUAGE TemplateHaskell #-}

-- | Description: The 'Except' effect, providing catch and throw functionality over the final
-- monad via MonadCatch.
module HaskellWorks.Polysemy.Except
  ( -- * Effect
    Except (..),

    -- * Actions
    catchEx,
    throwEx,

    -- * Interpretations
    catchExToFinal,
    catchExToFinalIO,
  ) where

import           Control.Exception    (Exception (..))
import qualified Control.Monad.Catch  as X
import           HaskellWorks.Prelude
import           Polysemy
import           Polysemy.Final

------------------------------------------------------------------------------
-- | An effect capable of providing 'X.catch' and 'X.throwM' semantics. Interpreters for
-- this will successfully run the catch the exceptions thrown in the IO monad.
data Except m a where
  CatchEx
    :: Exception e
    => m a
    -> (e -> m a)
    -> Except m a

  ThrowEx
    :: Exception e
    => e
    -> Except m a

makeSem ''Except

------------------------------------------------------------------------------
-- | Run a 'Except' effect in terms of 'X.catch' and 'X.throwM' through final monad.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Except' effects
-- interpreted this way. See 'Final'.
catchExToFinal :: forall a r m. ()
  => X.MonadCatch m
  => X.MonadThrow m
  => Member (Final m) r
  => Sem (Except ': r) a
  -> Sem r a
catchExToFinal = interpretFinal $ \case
  CatchEx f h -> do
    s  <- getInitialStateS
    a  <- runS f
    h' <- bindS h
    pure $ X.catch a $ \e -> h' $ e <$ s
  ThrowEx e ->
    pure $ X.throwM e
{-# INLINE catchExToFinal #-}

------------------------------------------------------------------------------
-- | Run a 'Except' effect in terms of 'X.catch' and 'X.throwM' through final IO monad.
catchExToFinalIO :: forall a r. ()
  => Member (Final IO) r
  => Sem (Except ': r) a
  -> Sem r a
catchExToFinalIO = catchExToFinal
{-# INLINE catchExToFinalIO #-}
