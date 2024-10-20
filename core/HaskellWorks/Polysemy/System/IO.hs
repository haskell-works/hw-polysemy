module HaskellWorks.Polysemy.System.IO
  ( IO.BufferMode(..),
    IO.FilePath,
    IO.Handle,
    IO.HandlePosn,
    IO.IO,
    IO.IOMode(..),
    IO.NewlineMode(..),
    IO.SeekMode(..),
    IO.TextEncoding,

    String,

    IO.stdin,
    IO.stdout,
    IO.stderr,

    withFile,
    hClose,
    openFile,
    readFile,
    appendFile,
    writeFile,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    hSetBuffering,
    hGetBuffering,
    hFlush,
    hGetPosn,
    hSetPosn,
    hSeek,
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hIsSeekable,
    hIsTerminalDevice,
    hSetEcho,
    hGetEcho,
    hShow,
    hWaitForInput,
    hReady,
    hGetChar,
    hGetLine,
    hLookAhead,
    hGetContents,
    hGetContents',
    hPutChar,
    hPutStr,
    hPutStrLn,
    hPrint,

    interact,
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    getContents',
    readIO,
    readLn,

    withBinaryFile,
    openBinaryFile,

    hPutBuf,
    hGetBuf,
    hGetBufSome,
    hPutBufNonBlocking,
    hGetBufNonBlocking,
    openTempFile,
    openBinaryTempFile,
    openTempFileWithDefaultPermissions,
    openBinaryTempFileWithDefaultPermissions,
    hSetEncoding,
    hGetEncoding,

    IO.latin1,
    IO.utf8,
    IO.utf8_bom,
    IO.utf16,
    IO.utf16le,
    IO.utf16be,
    IO.utf32,
    IO.utf32le,
    IO.utf32be,
    IO.localeEncoding,
    IO.char8,

    mkTextEncoding,
    hSetNewlineMode,

    IO.nativeNewline,
    IO.noNewlineTranslation,
    IO.universalNewlineMode,
    IO.nativeNewlineMode,
  ) where

import qualified Control.Exception             as CE
import qualified Data.Text                     as Text
import           Foreign.Ptr                   (Ptr)
import qualified GHC.Stack                     as GHC
import           HaskellWorks.Polysemy.Prelude
import qualified System.IO                     as IO
import           System.IO                     (BufferMode, Handle, HandlePosn,
                                                IOMode, NewlineMode, SeekMode,
                                                TextEncoding)

import           Polysemy
import           Polysemy.Error
import           Polysemy.Log
import           Polysemy.Resource

-- | Open a file handle and run an action, closing the handle when done.
withFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => FilePath
  -> IOMode
  -> (Handle -> Sem r a)
  -> Sem r a
withFile fp mode f = GHC.withFrozenCallStack $ do
  info $ "Calling: withFile " <> tshow fp <> " " <> tshow mode

  bracket (openFile fp mode) hClose f

-- | Open a file handle and run an action, closing the handle when done.
withBinaryFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => FilePath
  -> IOMode
  -> (Handle -> Sem r a)
  -> Sem r a
withBinaryFile fp mode f = GHC.withFrozenCallStack $ do
  info $ "Calling: withBinaryFile " <> tshow fp <> " " <> tshow mode

  bracket (openBinaryFile fp mode) hClose f

-- | Open a file handle.
openFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> IOMode
  -> Sem r Handle
openFile fp mode = GHC.withFrozenCallStack $ do
  r <- embed $ CE.try @IOException $ IO.openFile fp mode
  case r of
    Left e  -> do
      error $ "Failed to open file: " <> Text.pack fp
      throw e
    Right h -> do
      info $ "Opened file: " <> Text.pack fp
      pure h

-- | Open a file handle.
openBinaryFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> IOMode
  -> Sem r Handle
openBinaryFile fp mode = GHC.withFrozenCallStack $ do
  r <- embed $ CE.try @IOException $ IO.openBinaryFile fp mode
  case r of
    Left e  -> do
      error $ "Failed to open file: " <> Text.pack fp
      throw e
    Right h -> do
      info $ "Opened file: " <> Text.pack fp
      pure h

-- | Close the file handle.
hClose :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r ()
hClose h = GHC.withFrozenCallStack $ do
  info $ "Closing file: " <> tshow h
  fromEither =<< embed (CE.try @IOException $ IO.hClose h)

-- | Read a file as a string.
readFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r String
readFile filePath = GHC.withFrozenCallStack $ do
  info $ "Reading string file: " <> Text.pack filePath
  fromEither =<< embed (CE.try @IOException $ IO.readFile filePath)

writeFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r ()
writeFile fp contents = GHC.withFrozenCallStack $ do
  info $ "Write string to file: " <> Text.pack fp
  fromEither =<< embed (CE.try @IOException $ IO.writeFile fp contents)

appendFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r ()
appendFile fp contents = GHC.withFrozenCallStack $ do
  info $ "Append string to file: " <> Text.pack fp
  fromEither =<< embed (CE.try @IOException $ IO.appendFile fp contents)

hFileSize :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Integer
hFileSize h = GHC.withFrozenCallStack $ do
  info $ "Called hFileSize: " <> tshow h
  fromEither =<< embed (CE.try @IOException $ IO.hFileSize h)

hSetFileSize :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Integer
  -> Sem r ()
hSetFileSize h n = GHC.withFrozenCallStack $ do
  info $ "Called hSetFileSize: " <> tshow h <> " " <> tshow n
  fromEither =<< embed (CE.try @IOException $ IO.hSetFileSize h n)

hIsEOF :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsEOF h = GHC.withFrozenCallStack $ do
  info $ "Called hIsEOF: " <> tshow h
  fromEither =<< embed (CE.try @IOException $ IO.hIsEOF h)

isEOF :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r Bool
isEOF = GHC.withFrozenCallStack $ do
  debug "Called isEOF"
  fromEither =<< embed (CE.try @IOException IO.isEOF)

hSetBuffering :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> BufferMode
  -> Sem r ()
hSetBuffering h bufferMode = GHC.withFrozenCallStack $ do
  debug "Called hSetBuffering"
  fromEither =<< embed (CE.try @IOException $ IO.hSetBuffering h bufferMode)

hGetBuffering :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r BufferMode
hGetBuffering h = GHC.withFrozenCallStack $ do
  debug "Called hGetBuffering"
  fromEither =<< embed (CE.try @IOException $ IO.hGetBuffering h)

hFlush :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r ()
hFlush h = GHC.withFrozenCallStack $ do
  debug "Called hFlush"
  fromEither =<< embed (CE.try @IOException $ IO.hFlush h)

hGetPosn :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r HandlePosn
hGetPosn h = GHC.withFrozenCallStack $ do
  debug "Called hGetPosn"
  fromEither =<< embed (CE.try @IOException $ IO.hGetPosn h)

hSetPosn :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => HandlePosn
  -> Sem r ()
hSetPosn hPosn = GHC.withFrozenCallStack $ do
  debug "Called hSetPosn"
  fromEither =<< embed (CE.try @IOException $ IO.hSetPosn hPosn)

hSeek :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> SeekMode
  -> Integer
  -> Sem r ()
hSeek h seekMode n = GHC.withFrozenCallStack $ do
  debug "Called hSeek"
  fromEither =<< embed (CE.try @IOException $ IO.hSeek h seekMode n)

hTell :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Integer
hTell h = GHC.withFrozenCallStack $ do
  debug "Called hGetBuffering"
  fromEither =<< embed (CE.try @IOException $ IO.hTell h)

hIsOpen :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsOpen h = GHC.withFrozenCallStack $ do
  debug "Called hIsOpen"
  fromEither =<< embed (CE.try @IOException $ IO.hIsOpen h)

hIsClosed :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsClosed h = GHC.withFrozenCallStack $ do
  debug "Called hIsClosed"
  fromEither =<< embed (CE.try @IOException $ IO.hIsClosed h)

hIsReadable :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsReadable h = GHC.withFrozenCallStack $ do
  debug "Called hIsReadable"
  fromEither =<< embed (CE.try @IOException $ IO.hIsReadable h)

hIsWritable :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsWritable h = GHC.withFrozenCallStack $ do
  debug "Called hIsWritable"
  fromEither =<< embed (CE.try @IOException $ IO.hIsWritable h)

hIsSeekable :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsSeekable h = GHC.withFrozenCallStack $ do
  debug "Called hIsSeekable"
  fromEither =<< embed (CE.try @IOException $ IO.hIsSeekable h)

hIsTerminalDevice :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hIsTerminalDevice h = GHC.withFrozenCallStack $ do
  debug "Called hIsTerminalDevice"
  fromEither =<< embed (CE.try @IOException $ IO.hIsTerminalDevice h)

hSetEcho :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Bool
  -> Sem r ()
hSetEcho h v = GHC.withFrozenCallStack $ do
  debug "Called hSetEcho"
  fromEither =<< embed (CE.try @IOException $ IO.hSetEcho h v)

hGetEcho :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hGetEcho h = GHC.withFrozenCallStack $ do
  debug "Called hGetBuffering"
  fromEither =<< embed (CE.try @IOException $ IO.hGetEcho h)

hShow :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r String
hShow h = GHC.withFrozenCallStack $ do
  debug "Called hShow"
  fromEither =<< embed (CE.try @IOException $ IO.hShow h)

hWaitForInput :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Int
  -> Sem r Bool
hWaitForInput h n = GHC.withFrozenCallStack $ do
  debug "Called hWaitForInput"
  fromEither =<< embed (CE.try @IOException $ IO.hWaitForInput h n)

hReady :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Bool
hReady h = GHC.withFrozenCallStack $ do
  debug "Called hReady"
  fromEither =<< embed (CE.try @IOException $ IO.hReady h)

hGetChar :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Char
hGetChar h = GHC.withFrozenCallStack $ do
  debug "Called hGetChar"
  fromEither =<< embed (CE.try @IOException $ IO.hGetChar h)

hGetLine :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r String
hGetLine h = GHC.withFrozenCallStack $ do
  debug "Called hGetLine"
  fromEither =<< embed (CE.try @IOException $ IO.hGetLine h)

hLookAhead :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r Char
hLookAhead h = GHC.withFrozenCallStack $ do
  debug "Called hLookAhead"
  fromEither =<< embed (CE.try @IOException $ IO.hLookAhead h)

hGetContents :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r String
hGetContents h = GHC.withFrozenCallStack $ do
  debug "Called hGetContents"
  fromEither =<< embed (CE.try @IOException $ IO.hGetContents h)

hGetContents' :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r String
hGetContents' h = GHC.withFrozenCallStack $ do
  debug "Called hGetContents'"
  fromEither =<< embed (CE.try @IOException $ IO.hGetContents' h)

hPutChar :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Char
  -> Sem r ()
hPutChar h c = GHC.withFrozenCallStack $ do
  debug "Called hPutChar"
  fromEither =<< embed (CE.try @IOException $ IO.hPutChar h c)

hPutStr :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> String
  -> Sem r ()
hPutStr h s = GHC.withFrozenCallStack $ do
  debug "Called hPutStr"
  fromEither =<< embed (CE.try @IOException $ IO.hPutStr h s)

hPutStrLn :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> String
  -> Sem r ()
hPutStrLn h s = GHC.withFrozenCallStack $ do
  debug "Called hPutStrLn"
  fromEither =<< embed (CE.try @IOException $ IO.hPutStrLn h s)

hPrint :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> String
  -> Sem r ()
hPrint h s = GHC.withFrozenCallStack $ do
  debug "Called hPrint"
  fromEither =<< embed (CE.try @IOException $ IO.hPrint h s)

interact :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => (String -> String)
  -> Sem r ()
interact f = GHC.withFrozenCallStack $ do
  debug "Called interact"
  fromEither =<< embed (CE.try @IOException $ IO.interact f)

putChar :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Char
  -> Sem r ()
putChar c = GHC.withFrozenCallStack $ do
  debug "Called putChar"
  fromEither =<< embed (CE.try @IOException $ IO.putChar c)

putStr :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r ()
putStr s = GHC.withFrozenCallStack $ do
  debug "Called putStr"
  fromEither =<< embed (CE.try @IOException $ IO.putStr s)

putStrLn :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r ()
putStrLn s = GHC.withFrozenCallStack $ do
  debug "Called putStrLn"
  fromEither =<< embed (CE.try @IOException $ IO.putStrLn s)

print :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r ()
print s = GHC.withFrozenCallStack $ do
  debug "Called print"
  fromEither =<< embed (CE.try @IOException $ IO.print s)

getChar :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r Char
getChar = GHC.withFrozenCallStack $ do
  debug "Called getChar"
  fromEither =<< embed (CE.try @IOException IO.getChar)

getLine :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r String
getLine = GHC.withFrozenCallStack $ do
  debug "Called getLine"
  fromEither =<< embed (CE.try @IOException IO.getLine)

getContents :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r String
getContents = GHC.withFrozenCallStack $ do
  debug "Called getContents"
  fromEither =<< embed (CE.try @IOException IO.getContents)

getContents' :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r String
getContents' = GHC.withFrozenCallStack $ do
  debug "Called getContents'"
  fromEither =<< embed (CE.try @IOException IO.getContents')

readIO :: ()
  => GHC.HasCallStack
  => Read a
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r a
readIO s = GHC.withFrozenCallStack $ do
  debug "Called readIO"
  fromEither =<< embed (CE.try @IOException $ IO.readIO s)

readLn :: ()
  => GHC.HasCallStack
  => Read a
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r a
readLn = GHC.withFrozenCallStack $ do
  debug "Called readLn"
  fromEither =<< embed (CE.try @IOException IO.readLn)

hPutBuf :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Ptr a
  -> Int
  -> Sem r ()
hPutBuf h ptr n = GHC.withFrozenCallStack $ do
  debug "Called hPutBuf"
  fromEither =<< embed (CE.try @IOException $ IO.hPutBuf h ptr n)

hGetBuf :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Ptr a
  -> Int
  -> Sem r Int
hGetBuf h ptr n = GHC.withFrozenCallStack $ do
  debug "Called hGetBuf"
  fromEither =<< embed (CE.try @IOException $ IO.hGetBuf h ptr n)

hGetBufSome :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Ptr a
  -> Int
  -> Sem r Int
hGetBufSome h ptr n = GHC.withFrozenCallStack $ do
  debug "Called hGetBufSome"
  fromEither =<< embed (CE.try @IOException $ IO.hGetBufSome h ptr n)

hPutBufNonBlocking :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Ptr a
  -> Int
  -> Sem r Int
hPutBufNonBlocking h ptr n = GHC.withFrozenCallStack $ do
  debug "Called hPutBufNonBlocking"
  fromEither =<< embed (CE.try @IOException $ IO.hPutBufNonBlocking h ptr n)

hGetBufNonBlocking :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Ptr a
  -> Int
  -> Sem r Int
hGetBufNonBlocking h ptr n = GHC.withFrozenCallStack $ do
  debug "Called hGetBufNonBlocking"
  fromEither =<< embed (CE.try @IOException $ IO.hGetBufNonBlocking h ptr n)

openTempFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r (FilePath, Handle)
openTempFile fp s = GHC.withFrozenCallStack $ do
  debug "Called openTempFile"
  fromEither =<< embed (CE.try @IOException $ IO.openTempFile fp s)

openBinaryTempFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r (FilePath, Handle)
openBinaryTempFile fp s = GHC.withFrozenCallStack $ do
  debug "Called openBinaryTempFile"
  fromEither =<< embed (CE.try @IOException $ IO.openBinaryTempFile fp s)

openTempFileWithDefaultPermissions :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r (FilePath, Handle)
openTempFileWithDefaultPermissions fp s = GHC.withFrozenCallStack $ do
  debug "Called openTempFileWithDefaultPermissions"
  fromEither =<< embed (CE.try @IOException $ IO.openTempFileWithDefaultPermissions fp s)

openBinaryTempFileWithDefaultPermissions :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> String
  -> Sem r (FilePath, Handle)
openBinaryTempFileWithDefaultPermissions fp s = GHC.withFrozenCallStack $ do
  debug "Called openBinaryTempFileWithDefaultPermissions"
  fromEither =<< embed (CE.try @IOException $ IO.openBinaryTempFileWithDefaultPermissions fp s)

hSetEncoding :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> TextEncoding
  -> Sem r ()
hSetEncoding h enc = GHC.withFrozenCallStack $ do
  debug "Called hSetEncoding"
  fromEither =<< embed (CE.try @IOException $ IO.hSetEncoding h enc)

hGetEncoding :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> Sem r (Maybe TextEncoding)
hGetEncoding h = GHC.withFrozenCallStack $ do
  debug "Called hGetEncoding"
  fromEither =<< embed (CE.try @IOException $ IO.hGetEncoding h)

mkTextEncoding :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r TextEncoding
mkTextEncoding s = GHC.withFrozenCallStack $ do
  debug "Called mkTextEncoding"
  fromEither =<< embed (CE.try @IOException $ IO.mkTextEncoding s)

hSetNewlineMode :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Handle
  -> NewlineMode
  -> Sem r ()
hSetNewlineMode h nm = GHC.withFrozenCallStack $ do
  debug "Called hSetNewlineMode"
  fromEither =<< embed (CE.try @IOException $ IO.hSetNewlineMode h nm)
