module HaskellWorks.Polysemy.System.Directory
  ( D.XdgDirectory(..),
    D.XdgDirectoryList(..),

    -- * Actions on directories
    createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removePathForcibly,
    renameDirectory,
    listDirectory,
    getDirectoryContents,

    -- ** Current working directory
    getCurrentDirectory,
    setCurrentDirectory,
    withCurrentDirectory,

    -- * Pre-defined directories
    getHomeDirectory,
    getXdgDirectory,
    getXdgDirectoryList,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getTemporaryDirectory,

    -- * Actions on files
    removeFile,
    renameFile,
    renamePath,
    copyFile,
    copyFileWithMetadata,
    getFileSize,

    canonicalizePath,
    makeAbsolute,
    makeRelativeToCurrentDirectory,

    -- * Existence tests
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,

    findExecutable,
    findExecutables,
    findExecutablesInDirectories,
    findFile,
    findFiles,
    findFileWith,
    findFilesWith,
    D.exeExtension,

    -- * Symbolic links
    createFileLink,
    createDirectoryLink,
    removeDirectoryLink,
    pathIsSymbolicLink,
    getSymbolicLinkTarget,

    -- -- * Permissions

    D.Permissions,
    D.emptyPermissions,
    D.readable,
    D.writable,
    D.executable,
    D.searchable,
    D.setOwnerReadable,
    D.setOwnerWritable,
    D.setOwnerExecutable,
    D.setOwnerSearchable,

    getPermissions,
    setPermissions,
    copyPermissions,

    -- * Timestamps
    getAccessTime,
    getModificationTime,
    setAccessTime,
    setModificationTime,

    -- -- * Deprecated
    --   isSymbolicLink,
  ) where

import qualified Control.Exception             as CE
import qualified GHC.Stack                     as GHC
import           HaskellWorks.Polysemy.Prelude

import           Data.Time.Clock               (UTCTime)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log
import           Polysemy.Resource
import qualified System.Directory              as D
import           System.Directory              (XdgDirectory (..),
                                                XdgDirectoryList (..))

createDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
createDirectory fp = GHC.withFrozenCallStack $ do
  info $ "Calling: createDirectory " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.createDirectory fp)

createDirectoryIfMissing :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Bool
  -> FilePath
  -> Sem r ()
createDirectoryIfMissing includeParents fp = GHC.withFrozenCallStack $ do
  info $ "Calling: createDirectoryIfMissing " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.createDirectoryIfMissing includeParents fp)

removeDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
removeDirectory fp = GHC.withFrozenCallStack $ do
  info $ "Calling: removeDirectory " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.removeDirectory fp)

removeDirectoryRecursive :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
removeDirectoryRecursive fp = GHC.withFrozenCallStack $ do
  info $ "Calling: removeDirectoryRecursive " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.removeDirectoryRecursive fp)

removePathForcibly :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
removePathForcibly fp = GHC.withFrozenCallStack $ do
  info $ "Calling: removePathForcibly " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.removePathForcibly fp)

renameDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
renameDirectory old new = GHC.withFrozenCallStack $ do
  info $ "Calling: renameDirectory " <> tshow old <> " " <> tshow new

  fromEither =<< embed (CE.try @IOException $ D.renameDirectory old new)

listDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r [FilePath]
listDirectory fp = GHC.withFrozenCallStack $ do
  info $ "Calling: listDirectory " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.listDirectory fp)

getDirectoryContents :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r [FilePath]
getDirectoryContents fp = GHC.withFrozenCallStack $ do
  info $ "Calling: getDirectoryContents " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.getDirectoryContents fp)

getCurrentDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r FilePath
getCurrentDirectory = GHC.withFrozenCallStack $ do
  info "Calling: getCurrentDirectory"

  fromEither =<< embed (CE.try @IOException D.getCurrentDirectory)

setCurrentDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
setCurrentDirectory fp = GHC.withFrozenCallStack $ do
  info $ "Calling: setCurrentDirectory " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.setCurrentDirectory fp)

withCurrentDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Resource r
  => Member Log r
  => FilePath
  -> Sem r ()
  -> Sem r ()
withCurrentDirectory fp f = GHC.withFrozenCallStack $ do
  info $ "Calling: withCurrentDirectory " <> tshow fp

  bracket getCurrentDirectory setCurrentDirectory $ const $ setCurrentDirectory fp >> f

getHomeDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r FilePath
getHomeDirectory = GHC.withFrozenCallStack $ do
  info "Calling: getHomeDirectory"

  fromEither =<< embed (CE.try @IOException D.getHomeDirectory)

getXdgDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => XdgDirectory
  -> FilePath
  -> Sem r FilePath
getXdgDirectory dir fp = GHC.withFrozenCallStack $ do
  info $ "Calling: getXdgDirectory " <> tshow dir

  fromEither =<< embed (CE.try @IOException $ D.getXdgDirectory dir fp)

getXdgDirectoryList :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => XdgDirectoryList
  -> Sem r [FilePath]
getXdgDirectoryList list = GHC.withFrozenCallStack $ do
  info $ "Calling: getXdgDirectoryList " <> tshow list

  fromEither =<< embed (CE.try @IOException $ D.getXdgDirectoryList list)

getAppUserDataDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
getAppUserDataDirectory fp = GHC.withFrozenCallStack $ do
  info $ "Calling: getAppUserDataDirectory " <> tshow fp

  fromEither =<< embed (CE.try @IOException $ D.getAppUserDataDirectory fp)

getUserDocumentsDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r FilePath
getUserDocumentsDirectory = GHC.withFrozenCallStack $ do
  info "Calling: getUserDocumentsDirectory"

  fromEither =<< embed (CE.try @IOException D.getUserDocumentsDirectory)

getTemporaryDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => Sem r FilePath
getTemporaryDirectory = GHC.withFrozenCallStack $ do
  info "Calling: getTemporaryDirectory"

  fromEither =<< embed (CE.try @IOException D.getTemporaryDirectory)

removeFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
removeFile fp = GHC.withFrozenCallStack $ do
  info "Calling: removeFile"

  fromEither =<< embed (CE.try @IOException $ D.removeFile fp)

renameFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
renameFile old new = GHC.withFrozenCallStack $ do
  info "Calling: renameFile"

  fromEither =<< embed (CE.try @IOException $ D.renameFile old new)

renamePath :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
renamePath old new = GHC.withFrozenCallStack $ do
  info "Calling: renamePath"

  fromEither =<< embed (CE.try @IOException $ D.renamePath old new)

copyFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
copyFile src dst = GHC.withFrozenCallStack $ do
  info "Calling: copyFile"

  fromEither =<< embed (CE.try @IOException $ D.copyFile src dst)

copyFileWithMetadata :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
copyFileWithMetadata src dst = GHC.withFrozenCallStack $ do
  info "Calling: copyFileWithMetadata"

  fromEither =<< embed (CE.try @IOException $ D.copyFileWithMetadata src dst)

getFileSize :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r Integer
getFileSize fp = GHC.withFrozenCallStack $ do
  info "Calling: getFileSize"

  fromEither =<< embed (CE.try @IOException $ D.getFileSize fp)

canonicalizePath :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
canonicalizePath fp = GHC.withFrozenCallStack $ do
  info "Calling: canonicalizePath"

  fromEither =<< embed (CE.try @IOException $ D.canonicalizePath fp)

makeAbsolute :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
makeAbsolute fp = GHC.withFrozenCallStack $ do
  info "Calling: makeAbsolute"

  fromEither =<< embed (CE.try @IOException $ D.makeAbsolute fp)

makeRelativeToCurrentDirectory :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
makeRelativeToCurrentDirectory fp = GHC.withFrozenCallStack $ do
  info "Calling: makeRelativeToCurrentDirectory"

  fromEither =<< embed (CE.try @IOException $ D.makeRelativeToCurrentDirectory fp)

doesPathExist :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r Bool
doesPathExist fp = GHC.withFrozenCallStack $ do
  info "Calling: doesPathExist"

  fromEither =<< embed (CE.try @IOException $ D.doesPathExist fp)

doesFileExist :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r Bool
doesFileExist fp = GHC.withFrozenCallStack $ do
  info "Calling: doesFileExist"

  fromEither =<< embed (CE.try @IOException $ D.doesFileExist fp)

doesDirectoryExist :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r Bool
doesDirectoryExist fp = GHC.withFrozenCallStack $ do
  info "Calling: doesDirectoryExist"

  fromEither =<< embed (CE.try @IOException $ D.doesDirectoryExist fp)

findExecutable :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r (Maybe FilePath)
findExecutable fp = GHC.withFrozenCallStack $ do
  info "Calling: findExecutable"

  fromEither =<< embed (CE.try @IOException $ D.findExecutable fp)

findExecutables :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => String
  -> Sem r [FilePath]
findExecutables fp = GHC.withFrozenCallStack $ do
  info "Calling: findExecutables"

  fromEither =<< embed (CE.try @IOException $ D.findExecutables fp)

findExecutablesInDirectories :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => [FilePath]
  -> String
  -> Sem r [FilePath]
findExecutablesInDirectories fps fp = GHC.withFrozenCallStack $ do
  info "Calling: findExecutablesInDirectories"

  fromEither =<< embed (CE.try @IOException $ D.findExecutablesInDirectories fps fp)

findFile :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => [FilePath]
  -> String
  -> Sem r (Maybe FilePath)
findFile fps fp = GHC.withFrozenCallStack $ do
  info "Calling: findFile"

  fromEither =<< embed (CE.try @IOException $ D.findFile fps fp)

findFiles :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => [FilePath]
  -> String
  -> Sem r [FilePath]
findFiles fps fp = GHC.withFrozenCallStack $ do
  info "Calling: findFiles"

  fromEither =<< embed (CE.try @IOException $ D.findFiles fps fp)

findFileWith :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => (FilePath -> IO Bool)
  -> [FilePath]
  -> String
  -> Sem r (Maybe FilePath)
findFileWith p fps fp = GHC.withFrozenCallStack $ do
  info "Calling: findFileWith"

  fromEither =<< embed (CE.try @IOException $ D.findFileWith p fps fp)

findFilesWith :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => (FilePath -> IO Bool)
  -> [FilePath]
  -> String
  -> Sem r [FilePath]
findFilesWith p fps fp = GHC.withFrozenCallStack $ do
  info "Calling: findFilesWith"

  fromEither =<< embed (CE.try @IOException $ D.findFilesWith p fps fp)

createFileLink :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
createFileLink old new = GHC.withFrozenCallStack $ do
  info "Calling: createFileLink"

  fromEither =<< embed (CE.try @IOException $ D.createFileLink old new)

createDirectoryLink :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
createDirectoryLink old new = GHC.withFrozenCallStack $ do
  info "Calling: createDirectoryLink"

  fromEither =<< embed (CE.try @IOException $ D.createDirectoryLink old new)

removeDirectoryLink :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r ()
removeDirectoryLink fp = GHC.withFrozenCallStack $ do
  info "Calling: removeDirectoryLink"

  fromEither =<< embed (CE.try @IOException $ D.removeDirectoryLink fp)

pathIsSymbolicLink :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r Bool
pathIsSymbolicLink fp = GHC.withFrozenCallStack $ do
  info "Calling: pathIsSymbolicLink"

  fromEither =<< embed (CE.try @IOException $ D.pathIsSymbolicLink fp)

getSymbolicLinkTarget :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r FilePath
getSymbolicLinkTarget fp = GHC.withFrozenCallStack $ do
  info "Calling: getSymbolicLinkTarget"

  fromEither =<< embed (CE.try @IOException $ D.getSymbolicLinkTarget fp)

getPermissions :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r D.Permissions
getPermissions fp = GHC.withFrozenCallStack $ do
  info "Calling: getPermissions"

  fromEither =<< embed (CE.try @IOException $ D.getPermissions fp)

setPermissions :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> D.Permissions
  -> Sem r ()
setPermissions fp p = GHC.withFrozenCallStack $ do
  info "Calling: setPermissions"

  fromEither =<< embed (CE.try @IOException $ D.setPermissions fp p)

copyPermissions :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> FilePath
  -> Sem r ()
copyPermissions src dst = GHC.withFrozenCallStack $ do
  info "Calling: copyPermissions"

  fromEither =<< embed (CE.try @IOException $ D.copyPermissions src dst)

getAccessTime :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r UTCTime
getAccessTime fp = GHC.withFrozenCallStack $ do
  info "Calling: getAccessTime"

  fromEither =<< embed (CE.try @IOException $ D.getAccessTime fp)

getModificationTime :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> Sem r UTCTime
getModificationTime fp = GHC.withFrozenCallStack $ do
  info "Calling: getModificationTime"

  fromEither =<< embed (CE.try @IOException $ D.getModificationTime fp)

setAccessTime :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> UTCTime
  -> Sem r ()
setAccessTime fp t = GHC.withFrozenCallStack $ do
  info "Calling: setAccessTime"

  fromEither =<< embed (CE.try @IOException $ D.setAccessTime fp t)

setModificationTime :: ()
  => GHC.HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => Member Log r
  => FilePath
  -> UTCTime
  -> Sem r ()
setModificationTime fp t = GHC.withFrozenCallStack $ do
  info "Calling: setModificationTime"

  fromEither =<< embed (CE.try @IOException $ D.setModificationTime fp t)
