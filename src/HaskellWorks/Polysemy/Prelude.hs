module HaskellWorks.Polysemy.Prelude
  ( Bool(..)
  , Maybe(..)
  , Either(..)
  , String
  , Text
  , ByteString
  , Int
  , Int8
  , Int16
  , Int32
  , Int64
  , Word
  , Word8
  , Word16
  , Word32
  , Word64
  , FilePath

  , Eq(..)
  , Ord(..)
  , Show(..)
  , tshow

  , const
  , either
  , maybe
  , fst
  , snd
  , id
  , ($)
  , (&)
  , (.)
  , (</>)

  , void

  , for_

  , Monad(..)
  , Applicative(..)
  , Functor(..)
  , Bifunctor(..)
  , Semigroup(..)
  , Monoid(..)
  , Foldable(..)
  , Traversable(..)

  , IO

  , CallStack
  , HasCallStack
  , withFrozenCallStack

  , IOException
  , SomeException(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Bool
import           Data.ByteString
import           Data.Either
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.Text
import           Data.Traversable
import           Data.Tuple
import           Data.Word
import           GHC.Stack
import           System.FilePath
import           System.IO
import           Text.Show

import qualified Data.Text           as T

tshow :: Show a => a -> Text
tshow = T.pack . show
