module HaskellWorks.Polysemy.FilePath
  ( exeSuffix,
    addExeSuffix,
  ) where

import qualified HaskellWorks.Polysemy.OS      as OS

import qualified Data.List                     as L
import           HaskellWorks.Polysemy.Prelude

exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""

addExeSuffix :: String -> String
addExeSuffix s = if ".exe" `L.isSuffixOf` s
  then s
  else s <> exeSuffix
