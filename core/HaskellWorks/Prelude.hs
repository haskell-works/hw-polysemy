module HaskellWorks.Prelude
  ( Bool(..)
  , Char(..)
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
  , Integer
  , Word
  , Word8
  , Word16
  , Word32
  , Word64
  , Float
  , Double
  , FilePath
  , Void

  , Eq(..)
  , Ord(..)
  , Num(..)
  , Show(..)
  , IsString(..)
  , tshow

  , bool
  , const

  , absurd
  , vacuous

  , either
  , lefts
  , rights
  , isLeft
  , isRight
  , fromLeft
  , fromRight

  , maybe
  , isJust
  , isNothing
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe

  , fst
  , flip
  , snd
  , id
  , seq
  , curry
  , uncurry
  , ($!)
  , ($)
  , (&)
  , not
  , otherwise
  , (&&)
  , (||)
  , (.)
  , (</>)

  , void
  , mapM_
  , forM
  , forM_
  , sequence_
  , (=<<)
  , (>=>)
  , (<=<)
  , forever
  , join
  , msum
  , mfilter
  , filterM
  , foldM
  , foldM_
  , replicateM
  , replicateM_
  , guard
  , when
  , unless
  , liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  , ap
  , (<$!>)

  , (<$>)

  , (<**>)
  , liftA
  , liftA3
  , optional
  , asum

  , for_

  , Fractional(..)
  , Real(..)
  , RealFrac(..)
  , Ratio(..)
  , Rational
  , fromIntegral
  , realToFrac
  , even
  , odd
  , numericEnumFrom
  , numericEnumFromThen
  , numericEnumFromTo
  , numericEnumFromThenTo
  , integralEnumFrom
  , integralEnumFromThen
  , integralEnumFromTo
  , integralEnumFromThenTo
  , numerator
  , denominator
  , (%)

  , Monad(..)
  , MonadFail(..)
  , MonadPlus(..)
  , Applicative(..)
  , Alternative(..)
  , Contravariant(..)
  , Divisible(..)
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

  , Generic

  , IOException
  , SomeException(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.Bool
import           Data.ByteString
import           Data.Char
import           Data.Either
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.Text
import           Data.Traversable
import           Data.Tuple
import           Data.Void
import           Data.Word
import           GHC.Base
import           GHC.Generics
import           GHC.Num
import           GHC.Real
import           GHC.Stack
import           System.FilePath
import           Text.Show

import qualified Data.Text                            as T

tshow :: Show a => a -> Text
tshow = T.pack . show
