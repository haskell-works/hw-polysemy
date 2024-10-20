module HaskellWorks.Polysemy.Data.Text.Strict
  ( T.Text,

    -- * Creation and elimination
    T.pack,
    T.unpack,
    T.singleton,
    T.empty,

    -- * Basic interface
    T.length,
    T.compareLength,
    T.null,

    -- * Transformations
    T.map,
    T.intercalate,
    T.intersperse,
    T.transpose,
    T.reverse,
    T.replace,

    -- ** Case conversion
    -- $case
    T.toCaseFold,
    T.toLower,
    T.toUpper,
    T.toTitle,

    -- ** Justification
    T.justifyLeft,
    T.justifyRight,
    T.center,

    -- * Folds
    T.foldl,
    T.foldl',
    T.foldl1,
    T.foldl1',
    T.foldr,
    T.foldr',
    T.foldr1,

    -- ** Special folds
    T.concat,
    T.concatMap,
    T.any,
    T.all,
    T.maximum,
    T.minimum,
    T.isAscii,

    -- * Construction

    -- ** Scans
    T.scanl,
    T.scanl1,
    T.scanr,
    T.scanr1,

    -- ** Accumulating maps
    T.mapAccumL,
    T.mapAccumR,

    -- ** Generation and unfolding
    T.replicate,
    T.unfoldr,
    T.unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    T.take,
    T.takeEnd,
    T.drop,
    T.dropEnd,
    T.takeWhile,
    T.takeWhileEnd,
    T.dropWhile,
    T.dropWhileEnd,
    T.dropAround,
    T.strip,
    T.stripStart,
    T.stripEnd,
    T.splitAt,
    T.breakOn,
    T.breakOnEnd,
    T.break,
    T.span,
    T.spanM,
    T.spanEndM,
    T.group,
    T.groupBy,
    T.inits,
    T.tails,

    -- ** Breaking into many substrings
    -- $split
    T.splitOn,
    T.split,
    T.chunksOf,

    -- ** Breaking into lines and words
    T.lines,
    --, T.lines'
    T.words,
    T.unlines,
    T.unwords,

    -- * Predicates
    T.isPrefixOf,
    T.isSuffixOf,
    T.isInfixOf,

    -- ** View patterns
    T.stripPrefix,
    T.stripSuffix,
    T.commonPrefixes,

    -- * Searching
    T.filter,
    T.breakOnAll,
    T.find,
    T.elem,
    T.partition,

    -- * Indexing
    -- $index
    T.index,
    T.findIndex,
    T.count,

    -- * Zipping
    T.zip,
    T.zipWith,

    -- * File reading
    readFile,

  ) where

import qualified Control.Exception             as CE
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
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
  -> Sem r Text
readFile filePath =
  GHC.withFrozenCallStack $ do
    info $ "Reading text file: " <> T.pack filePath
    r <- embed $ CE.try @IOException $ T.readFile filePath
    fromEither r
