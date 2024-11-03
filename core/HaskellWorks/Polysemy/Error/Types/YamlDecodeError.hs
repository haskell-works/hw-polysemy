{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Polysemy.Error.Types.YamlDecodeError
  ( YamlDecodeError(..)
  ) where

import           HaskellWorks.Prelude

newtype YamlDecodeError =
  YamlDecodeError
  { message :: String
  }
  deriving (Eq, Generic, Show)
