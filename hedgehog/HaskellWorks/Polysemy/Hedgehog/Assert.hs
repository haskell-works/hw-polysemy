module HaskellWorks.Polysemy.Hedgehog.Assert
  ( Hedgehog,
    leftFail,
    leftFailM,
    leftFailWith,
    leftFailWithM,
    nothingFail,
    nothingFailM,
    requireHead,
    catchFail,
    trapFail,
    trapFailWith,
    evalIO,
    failure,
    failMessage,

    (===),

    assert,
    assertM,
    assertIO,
    assertWithinTolerance,
    assertPidOk,
    assertIsJsonFile_,
    assertIsYamlFile,
    assertFileExists,
    assertFilesExist,
    assertFileMissing,
    assertFilesMissing,
    assertFileOccurences,
    assertFileLines,
    assertEndsWithSingleNewline,
    assertDirectoryExists,
    assertDirectoryMissing,

    diff,
    failDiff,
  ) where


import           Control.Lens                                   ((^.))
import           Data.Aeson                                     (Value)
import           Data.Generics.Product.Any
import qualified Data.List                                      as L
import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Directory
import           HaskellWorks.Polysemy.System.IO                as IO
import           HaskellWorks.Polysemy.System.Process
import qualified Hedgehog.Internal.Property                     as H
import qualified Hedgehog.Internal.Show                         as H
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

(===) :: ()
  => HasCallStack
  => Member Hedgehog r
  => Eq a
  => Show a
  => a
  -> a
  -> Sem r ()
(===) a b = withFrozenCallStack $ assertEquals a b

-- | Fail when the result is Left.
leftFail :: ()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => Either e a
  -> Sem r a
leftFail r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> failMessage GHC.callStack (show e)

-- | Fail when the result is Left.
leftFailWith :: ()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => (e -> String)
  -> Either e a
  -> Sem r a
leftFailWith toS r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> failMessage GHC.callStack (toS e)

nothingFail :: ()
  => HasCallStack
  => Member Hedgehog r
  => Maybe a
  -> Sem r a
nothingFail r = withFrozenCallStack $ case r of
  Just a  -> return a
  Nothing -> failMessage GHC.callStack "Expected Just"

failure :: ()
  => HasCallStack
  => Member Hedgehog r
  => Sem r a
failure =
  withFrozenCallStack $ failWith Nothing ""

failMessage :: ()
  => HasCallStack
  => Member Hedgehog r
  => GHC.CallStack
  -> String
  -> Sem r a
failMessage cs =
  withFrozenCallStack $ failWithCustom cs Nothing

leftFailM :: forall e r a. ()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => Sem r (Either e a)
  -> Sem r a
leftFailM f =
  withFrozenCallStack $ f >>= leftFail

leftFailWithM :: forall e r a. ()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => (e -> String)
  -> Sem r (Either e a)
  -> Sem r a
leftFailWithM toS f =
  withFrozenCallStack $ f >>= leftFailWith toS

nothingFailM :: forall r a. ()
  => HasCallStack
  => Member Hedgehog r
  => Sem r (Maybe a)
  -> Sem r a
nothingFailM f =
  withFrozenCallStack $ f >>= nothingFail

catchFail :: forall e r a.()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => Sem (Error e ': r) a
  -> Sem r a
catchFail f =
  withFrozenCallStack $ f & runError & leftFailM
{-# DEPRECATED catchFail "Use trapFail instead" #-}

trapFail :: forall e r a.()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => Sem (Error e ': r) a
  -> Sem r a
trapFail f =
  withFrozenCallStack $ f & runError & leftFailM

trapFailWith :: forall e r a.()
  => HasCallStack
  => Member Hedgehog r
  => Show e
  => (e -> String)
  -> Sem (Error e ': r) a
  -> Sem r a
trapFailWith toS f =
  withFrozenCallStack $ f & runError & leftFailWithM toS

requireHead :: ()
  => HasCallStack
  => Member Hedgehog r
  => [a]
  -> Sem r a
requireHead = withFrozenCallStack $
  \case
    []    -> failMessage GHC.callStack "Cannot take head of empty list"
    (x:_) -> pure x

assert :: ()
  => HasCallStack
  => Member Hedgehog r
  => Bool
  -> Sem r ()
assert b = withFrozenCallStack $
  unless b $ failMessage GHC.callStack "Assertion failed"

assertM :: ()
  => HasCallStack
  => Member Hedgehog r
  => Sem r Bool
  -> Sem r ()
assertM f = withFrozenCallStack $
  f >>= assert

assertIO :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => IO Bool
  -> Sem r ()
assertIO f = withFrozenCallStack $
  embed f >>= assert

assertWithinTolerance :: ()
  => HasCallStack
  => Member Hedgehog r
  => Show a
  => Ord a
  => Num a
  => a -- ^ tested value @v@
  -> a -- ^ expected value @c@
  -> a -- ^ tolerance range @r@
  -> Sem r ()
assertWithinTolerance v c r = withFrozenCallStack $ do
  diff v (>=) (c - r)
  diff v (<=) (c + r)

diff :: ()
  => Member Hedgehog r
  => Show a
  => Show b
  => HasCallStack
  => a
  -> (a -> b -> Bool)
  -> b
  -> Sem r ()
diff x op y =
  withFrozenCallStack $ do
    eval (x `op` y)
      & onFalseM_ (failDiff x y)
      & void

-- | Fails with an error that shows the difference between two values.
failDiff :: ()
  => HasCallStack
  => Member Hedgehog r
  => Show a
  => Show b
  => a
  -> b
  -> Sem r ()
failDiff x y =
  case H.valueDiff <$> H.mkValue x <*> H.mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWith Nothing $
          L.unlines
            [ "Failed"
            , "━━ lhs ━━"
            , H.showPretty x
            , "━━ rhs ━━"
            , H.showPretty y
            ]

    Just vdiff@(H.ValueSame _) ->
      withFrozenCallStack $
        failWith (Just $
          H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff) ""

    Just vdiff ->
      withFrozenCallStack $
        failWith (Just $
          H.Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff) ""

assertPidOk :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => ProcessHandle
  -> Sem r Pid
assertPidOk hProcess = withFrozenCallStack $
  nothingFailM $ getPid hProcess

-- | Assert the 'filePath' can be parsed as JSON.
assertIsJsonFile_ :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertIsJsonFile_ fp = withFrozenCallStack $ do
  void (readJsonFile @Value fp)
    & trap @IOException (failMessage GHC.callStack . show)
    & trap @JsonDecodeError (\e -> failMessage GHC.callStack (e ^. the @"message"))

-- | Assert the 'filePath' can be parsed as YAML.
assertIsYamlFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertIsYamlFile fp = withFrozenCallStack $ do
  void (readYamlFile @Value fp)
    & trap @IOException (failMessage GHC.callStack . show)
    & trap @JsonDecodeError (\e -> failMessage GHC.callStack (e ^. the @"message"))
    & trap @YamlDecodeError (\e -> failMessage GHC.callStack (e ^. the @"message"))

-- | Asserts that the given file exists.
assertFileExists :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertFileExists file = withFrozenCallStack $ do
  exists <- doesFileExist file
    & trap @IOException (const (pure False))
  unless exists $ failWithCustom GHC.callStack Nothing (file <> " has not been successfully created.")

-- | Asserts that all of the given files exist.
assertFilesExist :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => [FilePath]
  -> Sem r ()
assertFilesExist files =
  withFrozenCallStack $ for_ files assertFileExists

-- | Asserts that the given file is missing.
assertFileMissing :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertFileMissing file = withFrozenCallStack $ do
  exists <- doesFileExist file
    & trap @IOException (const (pure False))
  when exists $ failWithCustom GHC.callStack Nothing (file <> " should not have been created.")

-- | Asserts that all of the given files are missing.
assertFilesMissing :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => [FilePath]
  -> Sem r ()
assertFilesMissing files =
  withFrozenCallStack $ for_ files assertFileMissing

-- | Assert the file contains the given number of occurrences of the given string
assertFileOccurences :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => Int -> String -> FilePath -> Sem r ()
assertFileOccurences n s fp = withFrozenCallStack $ do
  contents <- readFile fp
    & trap @IOException (failMessage GHC.callStack . show)

  L.length (L.filter (s `L.isInfixOf`) (L.lines contents)) === n

-- | Assert the file contains the given number of occurrences of the given string
assertFileLines :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => (Int -> Bool)
  -> FilePath
  -> Sem r ()
assertFileLines p fp = withFrozenCallStack $ do
  contents <- readFile fp
    & trap @IOException (failMessage GHC.callStack . show)

  let lines = L.lines contents

  let len = case L.reverse lines of
        "":xs -> L.length xs
        xs    -> L.length xs

  unless (p len) $ do
    failWithCustom GHC.callStack Nothing (fp <> " has an unexpected number of lines")

-- | Assert the file contains the given number of occurrences of the given string
assertEndsWithSingleNewline :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertEndsWithSingleNewline fp = withFrozenCallStack $ do
  contents <- readFile fp
    & trap @IOException (failMessage GHC.callStack . show)

  case L.reverse contents of
    '\n':'\n':_ -> failWithCustom GHC.callStack Nothing (fp <> " ends with too many newlines.")
    '\n':_ -> return ()
    _ -> failWithCustom GHC.callStack Nothing (fp <> " must end with newline.")

-- | Asserts that the given directory exists.
assertDirectoryExists :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
assertDirectoryExists dir = withFrozenCallStack $ do
  exists <- doesDirectoryExist dir
    & trap @IOException (const (pure False))
  unless exists $ failWithCustom GHC.callStack Nothing ("Directory '" <> dir <> "' does not exist on the file system.")

-- | Asserts that the given directory is missing.
assertDirectoryMissing :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => FilePath
  -> Sem r ()
assertDirectoryMissing dir = withFrozenCallStack $ do
  exists <- doesDirectoryExist dir
    & trap @IOException (const (pure False))
  when exists $ failWithCustom GHC.callStack Nothing ("Directory '" <> dir <> "' does exist on the file system.")
