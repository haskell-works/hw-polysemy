{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs        #-}

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
    jotJson,
    jotJson_,
    jotJsonM,
    jotJsonM_,
    jotJsonPretty,
    jotJsonPretty_,
    jotJsonPrettyM,
    jotJsonPrettyM_,
    jotYaml,
    jotYaml_,
    jotYamlM,
    jotYamlM_,
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

    jotShowDataLog,
    jotShowDataLogLocal,
  ) where

import           Data.Aeson                                     (ToJSON)

import qualified Data.Aeson                                     as J
import qualified Data.Aeson.Encode.Pretty                       as J
import qualified Data.ByteString.Lazy                           as LBS
import qualified Data.Text                                      as T
import qualified Data.Text                                      as Text
import qualified Data.Text.Encoding                             as T
import qualified Data.Text.Encoding                             as Text
import qualified Data.Text.Lazy                                 as LT
import qualified Data.Text.Lazy.Encoding                        as LT
import qualified Data.Yaml                                      as Y
import qualified GHC.Stack                                      as GHC
import           HaskellWorks.Polysemy.Prelude

import qualified Hedgehog.Internal.Property                     as H
import qualified Hedgehog.Internal.Source                       as H

import           HaskellWorks.Polysemy
import           HaskellWorks.Polysemy.Hedgehog.Effect.Hedgehog
import           HaskellWorks.Polysemy.Hedgehog.Workspace.Types
import           HaskellWorks.Polysemy.String
import           Polysemy
import           Polysemy.Internal.Tactics                      (liftT)
import qualified Polysemy.Log.Effect.DataLog                    as Log

-- | Annotate the given string at the context supplied by the callstack.
jotWithCallstack :: forall r. ()
  => Member Hedgehog r
  => GHC.CallStack
  -> String
  -> Sem r ()
jotWithCallstack cs a =
  writeLog $ H.Annotation (H.getCaller cs) a

-- | Annotate with the given string.
jot :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => String
  -> Sem r String
jot a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack b
  return b

-- | Annotate the given string returning unit.
jot_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToString a
  => a
  -> Sem r ()
jot_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack $ toString a

-- | Annotate the given text returning unit.
jotText_ :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Text
  -> Sem r ()
jotText_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack $ Text.unpack a

-- | Annotate the given string in a monadic context.
jotM :: forall a r. ()
  => ToString a
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r a
  -> Sem r a
jotM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ toString b
  return b

jotBsUtf8M :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r ByteString
  -> Sem r ByteString
jotBsUtf8M a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ Text.unpack $ Text.decodeUtf8 b
  return b

jotLbsUtf8M :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r LBS.ByteString
  -> Sem r LBS.ByteString
jotLbsUtf8M a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 b
  return b

-- | Annotate the given string in a monadic context returning unit.
jotM_ :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Sem r String
  -> Sem r ()
jotM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack b
  return ()

-- | Annotate the given string in IO.
jotIO :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => IO String
  -> Sem r String
jotIO f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack a
  return a

-- | Annotate the given string in IO returning unit.
jotIO_ :: forall r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => IO String
  -> Sem r ()
jotIO_ f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack a
  return ()

-- | Annotate the given value.
jotShow :: forall a r. ()
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
jotShow_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => a
  -> Sem r ()
jotShow_ a = GHC.withFrozenCallStack $ jotWithCallstack GHC.callStack (show a)

-- | Annotate the given value in a monadic context.
jotShowM :: forall a r. ()
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
jotShowM_ :: forall a r. ()
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
jotShowIO :: forall a r. ()
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
jotShowIO_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => IO a
  -> Sem r ()
jotShowIO_ f = GHC.withFrozenCallStack $ do
  !a <- evalIO f
  jotWithCallstack GHC.callStack (show a)
  return ()

-- | Annotate the given value as JSON.
jotJson :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r a
jotJson a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
  return b

-- | Annotate the given value as JSON.
jotJson_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r ()
jotJson_ a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
  return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonM :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r a
jotJsonM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
  return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonM_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r ()
jotJsonM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
  return ()

-- | Annotate the given value as JSON.
jotJsonPretty :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r a
jotJsonPretty a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
  return b

-- | Annotate the given value as JSON.
jotJsonPretty_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r ()
jotJsonPretty_ a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
  return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r a
jotJsonPrettyM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
  return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r ()
jotJsonPrettyM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
  return ()

-- | Annotate the given value as JSON.
jotYaml :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r a
jotYaml a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
  return b

-- | Annotate the given value as JSON.
jotYaml_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => a
  -> Sem r ()
jotYaml_ a = GHC.withFrozenCallStack $ do
  !b <- eval a
  jotWithCallstack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
  return ()

-- | Annotate the given value as JSON in a monadic context.
jotYamlM :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r a
jotYamlM a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
  return b

-- | Annotate the given value as JSON in a monadic context.
jotYamlM_ :: forall a r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => ToJSON a
  => Sem r a
  -> Sem r ()
jotYamlM_ a = GHC.withFrozenCallStack $ do
  !b <- evalM a
  jotWithCallstack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
  return ()

-- | Annotate the each value in the given traversable.
jotEach :: forall a f r. ()
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
jotEach_ :: forall a f r. ()
  => Member Hedgehog r
  => GHC.HasCallStack
  => Show a
  => Traversable f
  => f a
  -> Sem r ()
jotEach_ as = GHC.withFrozenCallStack $ for_ as $ jotWithCallstack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context.
jotEachM :: forall a f r. ()
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
jotEachM_ :: forall a f r. ()
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
jotEachIO :: forall a f r. ()
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
jotEachIO_ :: forall a f r. ()
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
jotPkgInputFile :: forall r. ()
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
jotPkgGoldenFile :: forall r. ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader PackagePath) r
  => FilePath
  -> Sem r FilePath
jotPkgGoldenFile fp = withFrozenCallStack $ do
  PackagePath { filePath = pkgPath } <- ask
  jot_ $ pkgPath <> "/" <> fp
  return fp

jotRootInputFile :: forall r. ()
  => HasCallStack
  => Member Hedgehog r
  => Member (Reader ProjectRoot) r
  => FilePath
  -> Sem r FilePath
jotRootInputFile fp = withFrozenCallStack $ do
  ProjectRoot { filePath = pkgPath } <- ask
  jot $ pkgPath <> "/" <> fp

-- | Return the test file path after annotating it relative to the project root directory
jotTempFile :: forall r. ()
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

jotShowDataLog :: forall a r. ()
  => HasCallStack
  => Show a
  => Member Hedgehog r
  => InterpreterFor (DataLog a) r
jotShowDataLog =
  withFrozenCallStack $
    jotShowDataLogLocal id
{-# inline jotShowDataLog #-}

jotShowDataLogLocal :: forall a r. ()
  => HasCallStack
  => Show a
  => Member Hedgehog r
  => (a -> a)
  -> InterpreterFor (DataLog a) r
jotShowDataLogLocal context =
  withFrozenCallStack $
    interpretH \case
      Log.DataLog a ->
        liftT (jotShow_ (context a))
      Log.Local f ma ->
        raise . jotShowDataLogLocal (f . context) =<< runT ma
{-# inline jotShowDataLogLocal #-}
