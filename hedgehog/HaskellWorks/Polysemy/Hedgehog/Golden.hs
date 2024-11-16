-- | For the diff functions in this module: If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the golden file not exist it would be created.  If
-- RECREATE_GOLDEN_FILES is set to "1", then should the golden file exist it would
-- be recreated. If GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file
-- path will be logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten

module HaskellWorks.Polysemy.Hedgehog.Golden
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
    diffJsonVsGoldenFile,
    diffYamlVsGoldenFile,
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                                    as J
import           Data.Algorithm.Diff                           (PolyDiff (Both),
                                                                getGroupedDiff)
import           Data.Algorithm.DiffOutput                     (ppDiff)
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import           GHC.Stack                                     (callStack)
import           HaskellWorks.Polysemy.Hedgehog.Assert
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           System.FilePath                               (takeDirectory)

import qualified Control.Concurrent.QSem                       as IO
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.List                                     as List
import           Data.Yaml                                     as Y
import qualified HaskellWorks.Polysemy.Control.Concurrent.QSem as PIO
import           HaskellWorks.Polysemy.Data.ByteString         as PBS
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Directory        as PIO
import           HaskellWorks.Polysemy.System.IO               as PIO
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log
import           Polysemy.Resource
import qualified System.Environment                            as IO
import qualified System.IO.Unsafe                              as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

-- | The file to log whenever a golden file is referenced.
mGoldenFileLogFile :: Maybe FilePath
mGoldenFileLogFile = IO.unsafePerformIO $
  IO.lookupEnv "GOLDEN_FILE_LOG_FILE"

-- | Whether the test should create the golden files if the files do not exist.
createGoldenFiles :: Bool
createGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Whether the test should recreate the golden files if the files already exist.
recreateGoldenFiles :: Bool
recreateGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "RECREATE_GOLDEN_FILES"
  return $ value == Just "1"

writeGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r ()
writeGoldenFile goldenFile actualContent = withFrozenCallStack $ do
  jot_ $ "Creating golden file " <> goldenFile
  PIO.createDirectoryIfMissing True (takeDirectory goldenFile)
  PIO.writeFile goldenFile actualContent

writeByteStringGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> ByteString
  -> Sem r ()
writeByteStringGoldenFile goldenFile bs = withFrozenCallStack $ do
  jot_ $ "Creating golden file " <> goldenFile
  PIO.createDirectoryIfMissing True (takeDirectory goldenFile)
  PBS.writeFile goldenFile bs

reportGoldenFileMissing :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> Sem r ()
reportGoldenFileMissing goldenFile = withFrozenCallStack $ do
  jot_ $ unlines
    [ "Golden file " <> goldenFile <> " does not exist."
    , "To create it, run with CREATE_GOLDEN_FILES=1."
    , "To recreate it, run with RECREATE_GOLDEN_FILES=1."
    ]
  failure

checkAgainstGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> [String]
  -> Sem r ()
checkAgainstGoldenFile goldenFile actualLines = withFrozenCallStack $ do
  referenceLines <- List.lines <$> PIO.readFile goldenFile
  let difference = getGroupedDiff actualLines referenceLines
  case difference of
    []       -> pure ()
    [Both{}] -> pure ()
    _        -> do
      jot_ $ unlines
        [ "Golden test failed against golden file: " <> goldenFile
        , "To recreate golden file, run with RECREATE_GOLDEN_FILES=1."
        ]
      failMessage callStack $ ppDiff difference

-- | Diff contents against the golden file.
--
-- TODO: Improve the help output by saying the difference of
-- each input.
diffVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffVsGoldenFile actualContent goldenFile = withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    PIO.bracketQSem sem $
      PIO.appendFile logFile (goldenFile <> "\n")
        & trapFail @IOException

  fileExists <- PIO.doesFileExist goldenFile
    & trapFail @IOException

  if
    | recreateGoldenFiles -> writeGoldenFile goldenFile actualContent       & trapFail @IOException
    | fileExists          -> checkAgainstGoldenFile goldenFile actualLines  & trapFail @IOException
    | createGoldenFiles   -> writeGoldenFile goldenFile actualContent       & trapFail @IOException
    | otherwise           -> reportGoldenFileMissing goldenFile             & trapFail @IOException

  where
    actualLines = List.lines actualContent

-- | Diff utf8 bytestring contents against the golden file.
diffByteStringVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => ByteString -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffByteStringVsGoldenFile bs goldenFile = withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    PIO.bracketQSem sem $
      PIO.appendFile logFile (goldenFile <> "\n")
        & trapFail @IOException

  fileExists <- PIO.doesFileExist goldenFile
    & trapFail @IOException

  if
    | recreateGoldenFiles -> writeByteStringGoldenFile goldenFile bs        & trapFail @IOException
    | fileExists          -> checkAgainstGoldenFile goldenFile actualLines  & trapFail @IOException
    | createGoldenFiles   -> writeByteStringGoldenFile goldenFile bs        & trapFail @IOException
    | otherwise           -> reportGoldenFileMissing goldenFile             & trapFail @IOException

  where
    actualLines = List.lines $ T.unpack $ T.decodeUtf8 bs

-- | Diff JSON against the golden file.
diffJsonVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => ToJSON a
  => a -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffJsonVsGoldenFile a goldenFile = withFrozenCallStack $
  diffByteStringVsGoldenFile (LBS.toStrict (J.encode a)) goldenFile

-- | Diff YAML against the golden file.
diffYamlVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => ToJSON a
  => a -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffYamlVsGoldenFile a goldenFile = withFrozenCallStack $
  diffByteStringVsGoldenFile (Y.encode a) goldenFile

-- | Diff file against the golden file.
diffFileVsGoldenFile :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member Hedgehog r
  => Member Log r
  => Member Resource r
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffFileVsGoldenFile actualFile referenceFile = withFrozenCallStack $ do
  contents <- PIO.readFile actualFile
    & trapFail @IOException

  diffVsGoldenFile contents referenceFile
    & trapFail @IOException
