{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Polysemy.Hedgehog.Golden
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Algorithm.Diff                           (PolyDiff (Both),
                                                                getGroupedDiff)
import           Data.Algorithm.DiffOutput                     (ppDiff)
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           GHC.Stack                                     (callStack)
import           HaskellWorks.Polysemy.Hedgehog.Assert
import           HaskellWorks.Polysemy.Hedgehog.Jot
import           System.FilePath                               (takeDirectory)

import qualified Control.Concurrent.QSem                       as IO
import qualified Data.List                                     as List
import qualified GHC.Stack                                     as GHC
import qualified HaskellWorks.Polysemy.Control.Concurrent.QSem as PIO
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
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r ()
writeGoldenFile goldenFile actualContent = do
  jot_ $ "Creating golden file " <> goldenFile
  PIO.createDirectoryIfMissing True (takeDirectory goldenFile)
  PIO.writeFile goldenFile actualContent

reportGoldenFileMissing :: ()
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> Sem r ()
reportGoldenFileMissing goldenFile = do
  jot_ $ unlines
    [ "Golden file " <> goldenFile <> " does not exist."
    , "To create it, run with CREATE_GOLDEN_FILES=1."
    , "To recreate it, run with RECREATE_GOLDEN_FILES=1."
    ]
  failure

checkAgainstGoldenFile :: ()
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => FilePath
  -> [String]
  -> Sem r ()
checkAgainstGoldenFile goldenFile actualLines = do
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

-- | Diff contents against the golden file.  If CREATE_GOLDEN_FILES environment is
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
-- files are never overwritten.
--
-- TODO: Improve the help output by saying the difference of
-- each input.
diffVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Resource r
  => Member Log r
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffVsGoldenFile actualContent goldenFile = GHC.withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    PIO.bracketQSem sem $ PIO.appendFile logFile $ goldenFile <> "\n"

  fileExists <- PIO.doesFileExist goldenFile

  if
    | recreateGoldenFiles -> writeGoldenFile goldenFile actualContent
    | fileExists          -> checkAgainstGoldenFile goldenFile actualLines
    | createGoldenFiles   -> writeGoldenFile goldenFile actualContent
    | otherwise           -> reportGoldenFileMissing goldenFile

  where
    actualLines = List.lines actualContent

-- | Diff file against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created.  If
-- GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file path will be
-- logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
diffFileVsGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => Member Resource r
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> Sem r ()
diffFileVsGoldenFile actualFile referenceFile = GHC.withFrozenCallStack $ do
  contents <- PIO.readFile actualFile
  diffVsGoldenFile contents referenceFile
