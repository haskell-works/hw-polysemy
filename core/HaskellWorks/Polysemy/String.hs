module HaskellWorks.Polysemy.String
  ( ToString(..)
  ) where


import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           HaskellWorks.Polysemy.Prelude

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Text where
  toString = T.unpack

instance ToString LT.Text where
  toString = LT.unpack
