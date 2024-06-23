module HaskellWorks.Polysemy.Data.ByteString.Lazy
  ( -- * Lazy @ByteString@
    LBS.ByteString,

    -- * Introducing and eliminating 'ByteString's
    LBS.empty,
    LBS.singleton,
    LBS.pack,
    LBS.unpack,
    LBS.fromStrict,
    LBS.toStrict,

    -- * Basic interface
    LBS.cons,
    LBS.snoc,
    LBS.append,
    LBS.head,
    LBS.uncons,
    LBS.unsnoc,
    LBS.last,
    LBS.tail,
    LBS.init,
    LBS.null,
    LBS.length,

    -- * Transforming ByteStrings
    LBS.map,
    LBS.reverse,
    LBS.intersperse,
    LBS.intercalate,
    LBS.transpose,

    -- * Reducing 'ByteString's (folds)
    LBS.foldl,
    LBS.foldl',
    LBS.foldl1,
    LBS.foldl1',

    LBS.foldr,
    LBS.foldr',
    LBS.foldr1,
    LBS.foldr1',

    -- ** Special folds
    LBS.concat,
    LBS.concatMap,
    LBS.any,
    LBS.all,
    LBS.maximum,
    LBS.minimum,

    -- * Building ByteStrings
    -- ** Scans
    LBS.scanl,
    LBS.scanl1,
    LBS.scanr,
    LBS.scanr1,

    -- ** Accumulating maps
    LBS.mapAccumL,
    LBS.mapAccumR,

    -- ** Generating and unfolding ByteStrings
    LBS.replicate,
    LBS.unfoldr,

    -- * Substrings

    -- ** Breaking strings
    LBS.take,
    LBS.takeEnd,
    LBS.drop,
    LBS.dropEnd,
    LBS.splitAt,
    LBS.takeWhile,
    LBS.takeWhileEnd,
    LBS.dropWhile,
    LBS.dropWhileEnd,
    LBS.span,
    LBS.spanEnd,
    LBS.break,
    LBS.breakEnd,
    LBS.group,
    LBS.groupBy,
    LBS.inits,
    LBS.tails,
    LBS.initsNE,
    LBS.tailsNE,
    LBS.stripPrefix,
    LBS.stripSuffix,

    -- ** Breaking into many substrings
    LBS.split,
    LBS.splitWith,

    -- * Predicates
    LBS.isPrefixOf,
    LBS.isSuffixOf,

    -- * Searching ByteStrings

    -- ** Searching by equality
    LBS.elem,
    LBS.notElem,

    -- ** Searching with a predicate
    LBS.find,
    LBS.filter,
    LBS.partition,

    -- * Indexing ByteStrings
    LBS.index,
    LBS.indexMaybe,
    (LBS.!?),
    LBS.elemIndex,
    LBS.elemIndices,
    LBS.elemIndexEnd,
    LBS.findIndex,
    LBS.findIndices,
    LBS.findIndexEnd,
    LBS.count,

    -- * Zipping and unzipping ByteStrings
    LBS.zip,
    LBS.zipWith,
    LBS.packZipWith,
    LBS.unzip,

    -- * Low level conversions
    -- ** Copying ByteStrings
    LBS.copy,

    -- * I\/O with 'ByteString's

    -- ** Standard input and output
    getContents,
    putStr,
    interact,

    -- ** Files
    readFile,
    writeFile,
    appendFile,

    -- ** I\/O with Handles
    hGetContents,
    hGet,
    hGetNonBlocking,
    hPut,
    hPutNonBlocking,
  ) where

import qualified Control.Exception             as CE
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           GHC.IO.Handle                 (Handle)
import           HaskellWorks.Polysemy.Prelude

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

getContents :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r LBS.ByteString
getContents = withFrozenCallStack $ do
  debug "Call to: getContents"
  r <- embed $ CE.try @IOException LBS.getContents
  fromEither r

putStr :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => LBS.ByteString
  -> Sem r ()
putStr bs = withFrozenCallStack $ do
  debug $ "Call to: putStr " <> T.decodeUtf8 (LBS.toStrict bs)
  r <- embed $ CE.try @IOException $ LBS.putStr bs
  fromEither r

interact :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => (LBS.ByteString -> LBS.ByteString)
  -> Sem r ()
interact f = withFrozenCallStack $ do
  debug "Call to: interact"
  r <- embed $ CE.try @IOException $ LBS.interact f
  fromEither r

readFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r LBS.ByteString
readFile filePath = withFrozenCallStack $ do
  info $ "Reading bytestring from file: " <> T.pack filePath
  r <- embed $ CE.try @IOException $ LBS.readFile filePath
  fromEither r

writeFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> LBS.ByteString
  -> Sem r ()
writeFile filePath bs = withFrozenCallStack $ do
  info $ "Writing bytestring to file: " <> T.pack filePath
  r <- embed $ CE.try @IOException $ LBS.writeFile filePath bs
  fromEither r

appendFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> LBS.ByteString
  -> Sem r ()
appendFile filePath bs = withFrozenCallStack $ do
  info $ "Appending bytestring to file: " <> T.pack filePath
  r <- embed $ CE.try @IOException $ LBS.appendFile filePath bs
  fromEither r

hGetContents :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r LBS.ByteString
hGetContents h = withFrozenCallStack $ do
  debug "Call to: hGetContents"
  r <- embed $ CE.try @IOException $ LBS.hGetContents h
  fromEither r

hGet :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r LBS.ByteString
hGet h n = withFrozenCallStack $ do
  debug "Call to: hGet"
  r <- embed $ CE.try @IOException $ LBS.hGet h n
  fromEither r

hGetNonBlocking :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r LBS.ByteString
hGetNonBlocking h n = withFrozenCallStack $ do
  debug "Call to: hGetNonBlocking"
  r <- embed $ CE.try @IOException $ LBS.hGetNonBlocking h n
  fromEither r

hPut :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> LBS.ByteString
  -> Sem r ()
hPut h bs = withFrozenCallStack $ do
  debug "Call to: hPut"
  r <- embed $ CE.try @IOException $ LBS.hPut h bs
  fromEither r

hPutNonBlocking :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> LBS.ByteString
  -> Sem r LBS.ByteString
hPutNonBlocking h bs = withFrozenCallStack $ do
  debug "Call to: hPutNonBlocking"
  r <- embed $ CE.try @IOException $ LBS.hPutNonBlocking h bs
  fromEither r
