module HaskellWorks.Polysemy.Hedgehog.Workspace.Types
  ( PackagePath(..),
    ProjectRoot(..),
    Workspace(..),
  ) where

import           GHC.Generics
import           HaskellWorks.Polysemy.Prelude

newtype Workspace = Workspace
  { filePath :: FilePath
  }
  deriving (Eq, Show, Generic)

newtype ProjectRoot = ProjectRoot
  { filePath :: FilePath
  }
  deriving (Eq, Show)

newtype PackagePath = PackagePath
  { filePath :: FilePath
  }
  deriving (Eq, Show)
