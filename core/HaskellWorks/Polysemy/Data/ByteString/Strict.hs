module HaskellWorks.Polysemy.Data.ByteString.Strict
  ( -- * Strict @ByteString@
    ByteString,

    -- * Introducing and eliminating 'ByteString's
    BS.empty,
    BS.singleton,
    BS.pack,
    BS.unpack,
    BS.fromStrict,
    BS.toStrict,
    fromFilePath,
    toFilePath,

    -- * Basic interface
    BS.cons,
    BS.snoc,
    BS.append,
    BS.head,
    BS.uncons,
    BS.unsnoc,
    BS.last,
    BS.tail,
    BS.init,
    BS.null,
    BS.length,

    -- * Transforming ByteStrings
    BS.map,
    BS.reverse,
    BS.intersperse,
    BS.intercalate,
    BS.transpose,

    -- * Reducing 'ByteString's (folds)
    BS.foldl,
    BS.foldl',
    BS.foldl1,
    BS.foldl1',

    BS.foldr,
    BS.foldr',
    BS.foldr1,
    BS.foldr1',

    -- ** Special folds
    BS.concat,
    BS.concatMap,
    BS.any,
    BS.all,
    BS.maximum,
    BS.minimum,

    -- * Building ByteStrings
    -- ** Scans
    BS.scanl,
    BS.scanl1,
    BS.scanr,
    BS.scanr1,

    -- ** Accumulating maps
    BS.mapAccumL,
    BS.mapAccumR,

    -- ** Generating and unfolding ByteStrings
    BS.replicate,
    BS.unfoldr,
    BS.unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    BS.take,
    BS.takeEnd,
    BS.drop,
    BS.dropEnd,
    BS.splitAt,
    BS.takeWhile,
    BS.takeWhileEnd,
    BS.dropWhile,
    BS.dropWhileEnd,
    BS.span,
    BS.spanEnd,
    BS.break,
    BS.breakEnd,
    BS.group,
    BS.groupBy,
    BS.inits,
    BS.tails,
    BS.initsNE,
    BS.tailsNE,
    BS.stripPrefix,
    BS.stripSuffix,

    -- ** Breaking into many substrings
    BS.split,
    BS.splitWith,

    -- * Predicates
    BS.isPrefixOf,
    BS.isSuffixOf,
    BS.isInfixOf,

    -- ** Encoding validation
    BS.isValidUtf8,

    -- ** Search for arbitrary substrings
    BS.breakSubstring,

    -- * Searching ByteStrings

    -- ** Searching by equality
    BS.elem,
    BS.notElem,

    -- ** Searching with a predicate
    BS.find,
    BS.filter,
    BS.partition,

    -- * Indexing ByteStrings
    BS.index,
    BS.indexMaybe,
    (BS.!?),
    BS.elemIndex,
    BS.elemIndices,
    BS.elemIndexEnd,
    BS.findIndex,
    BS.findIndices,
    BS.findIndexEnd,
    BS.count,

    -- * Zipping and unzipping ByteStrings
    BS.zip,
    BS.zipWith,
    BS.packZipWith,
    BS.unzip,

    -- * Ordered ByteStrings
    BS.sort,

    -- * Low level conversions
    -- ** Copying ByteStrings
    BS.copy,

    -- ** Packing 'CString's and pointers
    packCString,
    packCStringLen,

    -- * I\/O with 'ByteString's

    -- ** Standard input and output
    getLine,
    getContents,
    putStr,
    interact,

    -- ** Files
    readFile,
    writeFile,
    appendFile,

    -- ** I\/O with Handles
    hGetLine,
    hGetContents,
    hGet,
    hGetSome,
    hGetNonBlocking,
    hPut,
    hPutNonBlocking,
  ) where

import qualified Control.Exception             as CE
import qualified Data.ByteString               as BS
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Foreign                   (CString, CStringLen)
import           GHC.IO.Handle                 (Handle)
import           HaskellWorks.Polysemy.Prelude

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

fromFilePath :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ByteString
fromFilePath filePath = withFrozenCallStack $ do
  debug $ "Call to: fromFilePath " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ BS.fromFilePath filePath
  fromEither r

toFilePath :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => ByteString
  -> Sem r FilePath
toFilePath bs = withFrozenCallStack $ do
  debug $ "Call to: toFilePath " <> Text.decodeUtf8 bs
  r <- embed $ CE.try @IOException $ BS.toFilePath bs
  fromEither r

packCString :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => CString
  -> Sem r ByteString
packCString cs = withFrozenCallStack $ do
  debug $ "Call to: packCString " <> tshow cs
  r <- embed $ CE.try @IOException $ BS.packCString cs
  fromEither r

packCStringLen :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => CStringLen
  -> Sem r ByteString
packCStringLen csl = withFrozenCallStack $ do
  debug $ "Call to: packCStringLen " <> tshow csl
  r <- embed $ CE.try @IOException $ BS.packCStringLen csl
  fromEither r

getContents :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r ByteString
getContents = withFrozenCallStack $ do
  debug "Call to: getContents"
  r <- embed $ CE.try @IOException BS.getContents
  fromEither r

getLine :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r ByteString
getLine = withFrozenCallStack $ do
  debug "Call to: getLine"
  r <- embed $ CE.try @IOException BS.getLine
  fromEither r

putStr :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => ByteString
  -> Sem r ()
putStr bs = withFrozenCallStack $ do
  debug $ "Call to: putStr " <> Text.decodeUtf8 bs
  r <- embed $ CE.try @IOException $ BS.putStr bs
  fromEither r

interact :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => (ByteString -> ByteString)
  -> Sem r ()
interact f = withFrozenCallStack $ do
  debug "Call to: interact"
  r <- embed $ CE.try @IOException $ BS.interact f
  fromEither r

readFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ByteString
readFile filePath = withFrozenCallStack $ do
  info $ "Reading bytestring from file: " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ BS.readFile filePath
  fromEither r

writeFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> ByteString
  -> Sem r ()
writeFile filePath bs = withFrozenCallStack $ do
  info $ "Writing bytestring to file: " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ BS.writeFile filePath bs
  fromEither r

appendFile :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> ByteString
  -> Sem r ()
appendFile filePath bs = withFrozenCallStack $ do
  info $ "Appending bytestring to file: " <> Text.pack filePath
  r <- embed $ CE.try @IOException $ BS.appendFile filePath bs
  fromEither r

hGetLine :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r ByteString
hGetLine h = withFrozenCallStack $ do
  debug "Call to: hGetLine"
  r <- embed $ CE.try @IOException $ BS.hGetLine h
  fromEither r

hGetContents :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r ByteString
hGetContents h = withFrozenCallStack $ do
  debug "Call to: hGetContents"
  r <- embed $ CE.try @IOException $ BS.hGetContents h
  fromEither r

hGet :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r ByteString
hGet h n = withFrozenCallStack $ do
  debug "Call to: hGet"
  r <- embed $ CE.try @IOException $ BS.hGet h n
  fromEither r

hGetSome :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r ByteString
hGetSome h n = withFrozenCallStack $ do
  debug "Call to: hGetSome"
  r <- embed $ CE.try @IOException $ BS.hGetSome h n
  fromEither r

hGetNonBlocking :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r ByteString
hGetNonBlocking h n = withFrozenCallStack $ do
  debug "Call to: hGetNonBlocking"
  r <- embed $ CE.try @IOException $ BS.hGetNonBlocking h n
  fromEither r

hPut :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> ByteString
  -> Sem r ()
hPut h bs = withFrozenCallStack $ do
  debug "Call to: hPut"
  r <- embed $ CE.try @IOException $ BS.hPut h bs
  fromEither r

hPutNonBlocking :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> ByteString
  -> Sem r ByteString
hPutNonBlocking h bs = withFrozenCallStack $ do
  debug "Call to: hPutNonBlocking"
  r <- embed $ CE.try @IOException $ BS.hPutNonBlocking h bs
  fromEither r
