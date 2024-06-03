{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Polysemy.Hedgehog.Jot
  ( jotShow,
    jotShow_,
    jotWithCallstack,

    jot,
    jot_,
    jotText_,
    jotM,
    jotM_,
    jotBsUtf8M,
    jotLbsUtf8M,
    jotIO,
    jotIO_,
    jotShowM,
    jotShowM_,
    jotShowIO,
    jotShowIO_,
    jotEach,
    jotEach_,
    jotEachM,
    jotEachM_,
    jotEachIO,
    jotEachIO_,

    jotPkgGoldenFile,
    jotPkgInputFile,
    jotRootInputFile,
    jotTempFile,
  ) where


import qualified Data.ByteString.Lazy                           as LBS
import qualified Data.Text                                      as Text
import qualified Data.Text.Encoding                             as Text
import qualified Data.Text.Lazy                                 as LT
import qualified Data.Text.Lazy.Encoding                        as LT
import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import qualified Hedgehog.Internal.Property                     as H
import qualified Hedgehog.Internal.Source                       as H

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Workspace.Types
import           HaskellWorks.Polysemy.String

-- | Annotate the given string at the context supplied by the callstack.
jotWithCallstack :: ()
  => Member Hedgehog r
  => GHC.CallStack
  -> String
  -> Sem r ()
jotWithCallstack cs a =
  writeLog $ H.Annotation (H.getCaller cs) a

-- | Annotate with the given string.
jot :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => String
  -> Sem r String
jot a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack b
  return b

-- | Annotate the given string returning unit.
jot_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToString s
  => s
  -> Sem r ()
jot_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack $ toString a

-- | Annotate the given text returning unit.
jotText_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Text
  -> Sem r ()
jotText_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack $ Text.unpack a

-- | Annotate the given string in a monadic context.
jotM :: ()
  => ToString s
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r s
  -> Sem r s
jotM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ toString b
  return b

jotBsUtf8M :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r ByteString
  -> Sem r ByteString
jotBsUtf8M a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ Text.unpack $ Text.decodeUtf8 b
  return b

jotLbsUtf8M :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r LBS.ByteString
  -> Sem r LBS.ByteString
jotLbsUtf8M a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 b
  return b

-- | Annotate the given string in a monadic context returning unit.
jotM_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r String
  -> Sem r ()
jotM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack b
  return ()

-- | Annotate the given string in IO.
jotIO :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => IO String
  -> Sem r String
jotIO f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack a
  return a

-- | Annotate the given string in IO returning unit.
jotIO_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => IO String
  -> Sem r ()
jotIO_ f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack a
  return ()

-- | Annotate the given value.
jotShow :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => a
  -> Sem r a
jotShow a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value returning unit.
jotShow_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => a
  -> Sem r ()
jotShow_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack (show a)

-- | Annotate the given value in a monadic context.
jotShowM :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Sem r a
  -> Sem r a
jotShowM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack (show b)
  return b

-- | Annotate the given value in a monadic context returning unit.
jotShowM_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Sem r a
  -> Sem r ()
jotShowM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack (show b)
  return ()

-- | Annotate the given value in IO.
jotShowIO :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => IO a
  -> Sem r a
jotShowIO f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack (show a)
  return a

-- | Annotate the given value in IO returning unit.
jotShowIO_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => IO a
  -> Sem r ()
jotShowIO_ f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack (show a)
  return ()

-- | Annotate the each value in the given traversable.
jotEach :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => f a
  -> Sem r (f a)
jotEach as = GHC.withFrozenCallStack $ do
  for_ as $ jotWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable returning unit.
jotEach_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => f a
  -> Sem r ()
jotEach_ as = GHC.withFrozenCallStack $ for_ as $ jotWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context.
jotEachM :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => Sem r (f a)
  -> Sem r (f a)
jotEachM f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ jotWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in a monadic context returning unit.
jotEachM_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => Sem r (f a)
  -> Sem r ()
jotEachM_ f = GHC.withFrozenCallStack $ do
  !as <- f
  for_ as $ jotWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in IO.
jotEachIO :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => IO (f a)
  -> Sem r (f a)
jotEachIO f = GHC.withFrozenCallStack $ do
  !as <- evalIO f
  for_ as $ jotWithCallstack GHC.callStack . show
  return as

-- | Annotate the each value in the given traversable in IO returning unit.
jotEachIO_ :: ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => IO (f a)
  -> Sem r ()
jotEachIO_ f = GHC.withFrozenCallStack $ do
  !as <- evalIO f
  for_ as $ jotWithCallstack GHC.callStack . show

-- | Return the input file path after annotating it relative to the package directory
jotPkgInputFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader PackagePath) r
  => FilePath
  -> Sem r FilePath
jotPkgInputFile fp = withFrozenCallStack $ do
  PackagePath { filePath = pkgPath } <- ask
  jot_ $ pkgPath <> "/" <> fp
  return fp

-- | Return the golden file path after annotating it relative to the package directory
jotPkgGoldenFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader PackagePath) r
  => FilePath
  -> Sem r FilePath
jotPkgGoldenFile fp = withFrozenCallStack $ do
  PackagePath { filePath = pkgPath } <- ask
  jot_ $ pkgPath <> "/" <> fp
  return fp

jotRootInputFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader ProjectRoot) r
  => FilePath
  -> Sem r FilePath
jotRootInputFile fp = withFrozenCallStack $ do
  ProjectRoot { filePath = pkgPath } <- ask
  jot $ pkgPath <> "/" <> fp

-- | Return the test file path after annotating it relative to the project root directory
jotTempFile :: ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader Workspace) r
  => FilePath
  -> Sem r FilePath
jotTempFile fp = withFrozenCallStack $ do
  Workspace { filePath = workspace } <- ask
  let relPath = workspace <> "/" <> fp
  jot_ $ workspace <> "/" <> relPath
  return relPath
