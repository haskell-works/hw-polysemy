module HaskellWorks.Polysemy.Data.Text.Lazy
  ( LT.Text

  -- * Creation and elimination
  , LT.pack
  , LT.unpack
  , LT.singleton
  , LT.empty

  -- * Basic interface
  , LT.length
  , LT.compareLength

  -- * Transformations
  , LT.map
  , LT.intercalate
  , LT.intersperse
  , LT.transpose
  , LT.reverse
  , LT.replace

  -- ** Case conversion
  -- $case
  , LT.toCaseFold
  , LT.toLower
  , LT.toUpper
  , LT.toTitle

  -- ** Justification
  , LT.justifyLeft
  , LT.justifyRight
  , LT.center

  -- * Folds
  , LT.foldl
  , LT.foldl'
  , LT.foldl1
  , LT.foldl1'
  , LT.foldr
  , LT.foldr1

  -- ** Special folds
  , LT.concat
  , LT.concatMap
  , LT.any
  , LT.all
  , LT.maximum
  , LT.minimum
  , LT.isAscii

  -- * Construction

  -- ** Scans
  , LT.scanl
  , LT.scanl1
  , LT.scanr
  , LT.scanr1

  -- ** Accumulating maps
  , LT.mapAccumL
  , LT.mapAccumR

  -- ** Generation and unfolding
  , LT.replicate
  , LT.unfoldr
  , LT.unfoldrN

  -- * Substrings

  -- ** Breaking strings
  , LT.take
  , LT.takeEnd
  , LT.drop
  , LT.dropEnd
  , LT.takeWhile
  , LT.takeWhileEnd
  , LT.dropWhile
  , LT.dropWhileEnd
  , LT.dropAround
  , LT.strip
  , LT.stripStart
  , LT.stripEnd
  , LT.splitAt
  , LT.breakOn
  , LT.breakOnEnd
  , LT.break
  , LT.span
  , LT.spanM
  , LT.spanEndM
  , LT.group
  , LT.groupBy
  , LT.inits
  , LT.tails

  -- ** Breaking into many substrings
  -- $split
  , LT.splitOn
  , LT.split
  , LT.chunksOf

  -- ** Breaking into lines and words
  , LT.lines
  --, LT.lines'
  , LT.words
  , LT.unlines
  , LT.unwords

  -- * Predicates
  , LT.isPrefixOf
  , LT.isSuffixOf
  , LT.isInfixOf

  -- ** View patterns
  , LT.stripPrefix
  , LT.stripSuffix
  , LT.commonPrefixes

  -- * Searching
  , LT.filter
  , LT.breakOnAll
  , LT.find
  , LT.elem
  , LT.partition

  -- , LT.findSubstring

  -- * Indexing
  -- $index
  , LT.index
  , LT.count

  -- * Zipping
  , LT.zip
  , LT.zipWith

  -- * File reading
  , readFile

  ) where

import qualified Control.Exception             as CE
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LT
import qualified GHC.Stack                     as GHC
import           HaskellWorks.Polysemy.Prelude

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

-- | Read the contents of the 'filePath' file.
readFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r LT.Text
readFile filePath =
  GHC.withFrozenCallStack $ do
    info $ "Reading text file: " <> T.pack filePath
    r <- embed $ CE.try @IOException $ LT.readFile filePath
    fromEither r
