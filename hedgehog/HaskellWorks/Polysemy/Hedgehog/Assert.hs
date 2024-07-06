module HaskellWorks.Polysemy.Hedgehog.Assert
  ( Hedgehog,
    leftFail,
    leftFailM,
    leftFailJson,
    leftFailJsonM,
    leftFailJsonPretty,
    leftFailJsonPrettyM,
    leftFailYaml,
    leftFailYamlM,
    nothingFail,
    nothingFailM,
    requireHead,
    catchFail,
    trapFail,
    trapFailJson,
    trapFailJsonPretty,
    trapFailYaml,
    evalIO,
    failure,
    failMessage,
    byDeadlineIO,
    byDeadlineM,
    byDurationIO,
    byDurationM,

    (===),

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
  ) where


import qualified Control.Concurrent                             as IO
import           Control.Lens                                   ((^.))
import           Data.Aeson                                     (ToJSON, Value)
import qualified Data.Aeson                                     as J
import qualified Data.Aeson.Encode.Pretty                       as J
import           Data.Generics.Product.Any
import qualified Data.List                                      as L
import qualified Data.Text                                      as T
import qualified Data.Text.Encoding                             as T
import qualified Data.Text.Lazy                                 as LT
import qualified Data.Text.Lazy.Encoding                        as LT
import           Data.Time.Clock                                (NominalDiffTime,
                                                                 UTCTime)
import qualified Data.Time.Clock                                as DTC
import qualified Data.Yaml                                      as Y
import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Error
import           HaskellWorks.Polysemy.File
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Directory

import           HaskellWorks.Polysemy.System.IO                as IO
import           HaskellWorks.Polysemy.System.Process
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

(===) :: ()
  => Member Hedgehog r
  => Eq a
  => Show a
  => HasCallStack
  => a
  -> a
  -> Sem r ()
(===) a b = withFrozenCallStack $ assertEquals a b

-- | Fail when the result is Left.
leftFail :: ()
  => Member Hedgehog r
  => Show e
  => HasCallStack
  => Either e a
  -> Sem r a
leftFail r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> failMessage GHC.callStack ("Expected Right: " <> show e)

nothingFail :: ()
  => Member Hedgehog r
  => HasCallStack
  => Maybe a
  -> Sem r a
nothingFail r = withFrozenCallStack $ case r of
  Just a  -> return a
  Nothing -> failMessage GHC.callStack "Expected Just"

failure :: ()
  => Member Hedgehog r
  => HasCallStack
  => Sem r a
failure =
  withFrozenCallStack $ failWith Nothing ""

failMessage :: ()
  => Member Hedgehog r
  => HasCallStack
  => GHC.CallStack
  -> String
  -> Sem r a
failMessage cs =
  withFrozenCallStack $ failWithCustom cs Nothing

leftFailM :: forall e r a. ()
  => Member Hedgehog r
  => Show e
  => HasCallStack
  => Sem r (Either e a)
  -> Sem r a
leftFailM f =
  withFrozenCallStack $ f >>= leftFail

-- | Fail when the result is Left with the error message as JSON.
leftFailJson :: ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Either e a
  -> Sem r a
leftFailJson r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> do
    let msg = LT.unpack $ LT.decodeUtf8 $ J.encode e
    failMessage GHC.callStack ("Expected Right: " <> msg)

leftFailJsonM :: forall e r a. ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Sem r (Either e a)
  -> Sem r a
leftFailJsonM f =
  withFrozenCallStack $ f >>= leftFailJson

-- | Fail when the result is Left with the error message as JSON.
leftFailJsonPretty :: ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Either e a
  -> Sem r a
leftFailJsonPretty r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> do
    let msg = LT.unpack $ LT.decodeUtf8 $ J.encodePretty e
    failMessage GHC.callStack ("Expected Right: " <> msg)

leftFailJsonPrettyM :: forall e r a. ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Sem r (Either e a)
  -> Sem r a
leftFailJsonPrettyM f =
  withFrozenCallStack $ f >>= leftFailJsonPretty

-- | Fail when the result is Left with the error message as JSON.
leftFailYaml :: ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Either e a
  -> Sem r a
leftFailYaml r = withFrozenCallStack $ case r of
  Right a -> pure a
  Left e  -> do
    let msg = T.unpack $ T.decodeUtf8 $ Y.encode e
    failMessage GHC.callStack ("Expected Right: " <> msg)

leftFailYamlM :: forall e r a. ()
  => Member Hedgehog r
  => ToJSON e
  => HasCallStack
  => Sem r (Either e a)
  -> Sem r a
leftFailYamlM f =
  withFrozenCallStack $ f >>= leftFailYaml

nothingFailM :: forall r a. ()
  => Member Hedgehog r
  => HasCallStack
  => Sem r (Maybe a)
  -> Sem r a
nothingFailM f =
  withFrozenCallStack $ f >>= nothingFail

catchFail :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => Show e
  => Sem (Error e ': r) a
  -> Sem r a
catchFail f =
  withFrozenCallStack $ f & runError & leftFailM
{-# DEPRECATED catchFail "Use trapFail instead" #-}

trapFail :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => Show e
  => Sem (Error e ': r) a
  -> Sem r a
trapFail f = do
  r <- withFrozenCallStack $ f & runError
  case r of
    Right a -> pure a
    Left e  -> failMessage GHC.callStack $ show e

trapFailJson :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => ToJSON e
  => Sem (Error e ': r) a
  -> Sem r a
trapFailJson f = do
  r <- withFrozenCallStack $ f & runError
  case r of
    Right a -> pure a
    Left e  -> do
      let msg = LT.unpack $ LT.decodeUtf8 $ J.encode e
      failMessage GHC.callStack msg

trapFailJsonPretty :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => ToJSON e
  => Sem (Error e ': r) a
  -> Sem r a
trapFailJsonPretty f = do
  r <- withFrozenCallStack $ f & runError
  case r of
    Right a -> pure a
    Left e  -> do
      let msg = LT.unpack $ LT.decodeUtf8 $ J.encodePretty e
      failMessage GHC.callStack msg

trapFailYaml :: forall e r a.()
  => Member Hedgehog r
  => HasCallStack
  => ToJSON e
  => Sem (Error e ': r) a
  -> Sem r a
trapFailYaml f = do
  r <- withFrozenCallStack $ f & runError
  case r of
    Right a -> pure a
    Left e  -> do
      let msg = T.unpack $ T.decodeUtf8 $ Y.encode e
      failMessage GHC.callStack msg

requireHead :: ()
  => Member Hedgehog r
  => HasCallStack
  => [a]
  -> Sem r a
requireHead = withFrozenCallStack $
  \case
    []    -> failMessage GHC.callStack "Cannot take head of empty list"
    (x:_) -> pure x

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

byDeadlineIO :: ()
  => HasCallStack
  => Member (Embed m) r
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => NominalDiffTime
  -> UTCTime
  -> String
  -> m a
  -> Sem r a
byDeadlineIO period deadline errorMessage f = GHC.withFrozenCallStack $ byDeadlineM period deadline errorMessage $ embed f

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
byDeadlineM ::  ()
  => HasCallStack
  => Member Hedgehog r
  => Member Log r
  => Member (Embed IO) r
  => NominalDiffTime
  -> UTCTime
  -> String
  -> Sem r a
  -> Sem r a
byDeadlineM period deadline errorMessage f = GHC.withFrozenCallStack $ do
  start <- embed DTC.getCurrentTime
  a <- goM
  end <- embed DTC.getCurrentTime
  jot_ $ "Operation completed in " <> show (DTC.diffUTCTime end start)
  return a
  where goM = catchAssertion f $ \e -> do
          currentTime <- embed DTC.getCurrentTime
          if currentTime < deadline
            then do
              embed $ IO.threadDelay (floor (DTC.nominalDiffTimeToSeconds period * 1000000))
              goM
            else do
              jotShow_ currentTime
              void $ failMessage GHC.callStack $ "Condition not met by deadline: " <> errorMessage
              throwAssertion e

-- | Run the operation 'f' once a second until it returns 'True' or the duration expires.
--
-- Expiration of the duration results in an assertion failure
byDurationIO :: ()
  => HasCallStack
  => Member (Embed m) r
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => NominalDiffTime
  -> NominalDiffTime
  -> String
  -> m b
  -> Sem r b
byDurationIO period duration errorMessage f =
  GHC.withFrozenCallStack $ byDurationM period duration errorMessage $ embed f

-- | Run the operation 'f' once a second until it returns 'True' or the duration expires.
--
-- Expiration of the duration results in an assertion failure
byDurationM :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => NominalDiffTime
  -> NominalDiffTime
  -> String
  -> Sem r b
  -> Sem r b
byDurationM period duration errorMessage f = GHC.withFrozenCallStack $ do
  deadline <- DTC.addUTCTime duration <$> embed DTC.getCurrentTime
  byDeadlineM period deadline errorMessage f
