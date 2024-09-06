module Polsyemy.Control.Monad
  ( repeatNUntilM_,
    repeatNWhileM_,
  ) where

import           HaskellWorks.Prelude

-- | Repeat an action n times or until the action returns True.
repeatNUntilM_ :: ()
  => Monad m
  => Int
  -> (Int -> m Bool)
  -> m ()
repeatNUntilM_ 0 _ = return ()
repeatNUntilM_ n action = do
  shouldTerminate <- action n
  unless shouldTerminate $ repeatNUntilM_ (n - 1) action

-- | Repeat an action n times or while the action returns True.
repeatNWhileM_ :: ()
  => Monad m
  => Int
  -> (Int -> m Bool)
  -> m ()
repeatNWhileM_ 0 _ = return ()
repeatNWhileM_ n action = do
  shouldContinue <- action n
  when shouldContinue $ repeatNUntilM_ (n - 1) action
