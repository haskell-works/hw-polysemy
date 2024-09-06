module HaskellWorks.Control.Monad
  ( repeatNUntilM_,
    repeatNWhileM_,
  ) where

import           HaskellWorks.Prelude

-- | Repeat an action n times until the action returns True.
repeatNUntilM_ :: ()
  => Monad m
  => Int
  -> (Int -> m Bool)
  -> m ()
repeatNUntilM_ n action = go 0
  where
    go i =
      when (i < n) $ do
        shouldTerminate <- action i
        unless shouldTerminate $ go (i + 1)

-- | Repeat an action n times while the action returns True.
repeatNWhileM_ :: ()
  => Monad m
  => Int
  -> (Int -> m Bool)
  -> m ()
repeatNWhileM_ n action = go 0
  where
    go i =
      when (i < n) $ do
        shouldContinue <- action i
        when shouldContinue $ go (i + 1)
