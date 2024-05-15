module HaskellWorks.Polysemy.Data.Text
  ( Text

  -- * Creation and elimination
  , pack
  , unpack
  , singleton
  , empty

  -- * Basic interface
  , length
  , compareLength
  , null

  -- * Transformations
  , map
  , intercalate
  , intersperse
  , transpose
  , reverse
  , replace

  -- ** Case conversion
  -- $case
  , toCaseFold
  , toLower
  , toUpper
  , toTitle

  -- ** Justification
  , justifyLeft
  , justifyRight
  , center

  -- * Folds
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr'
  , foldr1

  -- ** Special folds
  , concat
  , concatMap
  , any
  , all
  , maximum
  , minimum
  , isAscii

  -- * Construction

  -- ** Scans
  , scanl
  , scanl1
  , scanr
  , scanr1

  -- ** Accumulating maps
  , mapAccumL
  , mapAccumR

  -- ** Generation and unfolding
  , replicate
  , unfoldr
  , unfoldrN

  -- * Substrings

  -- ** Breaking strings
  , take
  , takeEnd
  , drop
  , dropEnd
  , takeWhile
  , takeWhileEnd
  , dropWhile
  , dropWhileEnd
  , dropAround
  , strip
  , stripStart
  , stripEnd
  , splitAt
  , breakOn
  , breakOnEnd
  , break
  , span
  , spanM
  , spanEndM
  , group
  , groupBy
  , inits
  , tails

  -- ** Breaking into many substrings
  -- $split
  , splitOn
  , split
  , chunksOf

  -- ** Breaking into lines and words
  , lines
  --, lines'
  , words
  , unlines
  , unwords

  -- * Predicates
  , isPrefixOf
  , isSuffixOf
  , isInfixOf

  -- ** View patterns
  , stripPrefix
  , stripSuffix
  , commonPrefixes

  -- * Searching
  , filter
  , breakOnAll
  , find
  , elem
  , partition

  -- , findSubstring

  -- * Indexing
  -- $index
  , index
  , findIndex
  , count

  -- * Zipping
  , zip
  , zipWith

  -- * File reading
  , readFile

  ) where

import           HaskellWorks.Polysemy.Data.Text.Strict
