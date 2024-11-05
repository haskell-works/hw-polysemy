{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Polysemy.List (
    onMany,
    onManyM,
) where

import           Data.List.NonEmpty

import           HaskellWorks.Prelude
import           Polysemy

-- | Handle the case where a list with many (more than one) elements.
onMany :: (NonEmpty a -> Sem r (Maybe a)) -> [a] -> Sem r (Maybe a)
onMany h as = case as of
    []       -> pure Nothing
    [x]      -> pure (Just x)
    (x : xs) -> h (x :| xs)

-- | Handle the case where an effectul function returns a list with many (more than one) elements.
onManyM :: (NonEmpty a -> Sem r (Maybe a)) -> Sem r [a] -> Sem r (Maybe a)
onManyM h f =
  f >>= onMany h
