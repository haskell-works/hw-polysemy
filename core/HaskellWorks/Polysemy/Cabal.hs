module HaskellWorks.Polysemy.Cabal
  ( findDefaultPlanJsonFile
  , getPlanJsonFile
  , binDist
  ) where

import           HaskellWorks.Polysemy.Cabal.Types
import qualified HaskellWorks.Polysemy.Data.ByteString.Lazy as LBS
import           HaskellWorks.Polysemy.Error.Types
import           HaskellWorks.Polysemy.Prelude
import           HaskellWorks.Polysemy.System.Directory
import           HaskellWorks.Polysemy.System.Environment
import           System.FilePath                            (takeDirectory)

import           Data.Aeson
import qualified Data.List                                  as L
import qualified HaskellWorks.Polysemy.Data.Text            as T
import           HaskellWorks.Polysemy.FilePath
import           Polysemy
import           Polysemy.Error
import           Polysemy.Log

-- | Find the nearest plan.json going upwards from the current directory.
findDefaultPlanJsonFile :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => Sem r FilePath
findDefaultPlanJsonFile = getCurrentDirectory >>= go
  where go :: ()
          => Member (Embed IO) r
          => Member (Error IOException) r
          => Member Log r
          => FilePath
          -> Sem r FilePath
        go d = do
          let file = d </> "dist-newstyle/cache/plan.json"
          exists <- doesFileExist file
          if exists
            then return file
            else do
              let parent = takeDirectory d
              if parent == d
                then return "dist-newstyle/cache/plan.json"
                else go parent


getPlanJsonFile :: ()
  => Member (Embed IO) r
  => Member (Error IOException) r
  => Member Log r
  => Sem r String
getPlanJsonFile =  do
  maybeBuildDir <- lookupEnv "CABAL_BUILDDIR"
  case maybeBuildDir of
    Just buildDir -> return $ ".." </> buildDir </> "cache/plan.json"
    Nothing       -> findDefaultPlanJsonFile

-- | Consult the "plan.json" generated by cabal to get the path to the executable corresponding.
-- to a haskell package.  It is assumed that the project has already been configured and the
-- executable has been built.
binDist:: ()
  => Member (Embed IO) r
  => Member (Error GenericError) r
  => Member (Error IOException) r
  => Member Log r
  => String
  -- ^ Package name
  -> Sem r FilePath
  -- ^ Path to executable
binDist pkg = do
  planJsonFile <- getPlanJsonFile
  contents <- LBS.readFile planJsonFile

  case eitherDecode contents of
    Right plan -> case L.filter matching (plan & installPlan) of
      (component:_) -> case component & binFile of
        Just bin -> return $ addExeSuffix (T.unpack bin)
        Nothing  -> throw $ GenericError $ "Missing bin-file in " <> tshow component
      [] -> throw $ GenericError $ "Cannot find exe " <> tshow pkg <> " in plan"
    Left message -> throw $ GenericError $ "Cannot decode plan: " <> T.pack message
  where matching :: Component -> Bool
        matching component = case componentName component of
          Just name -> name == "exe:" <> T.pack pkg
          Nothing   -> False
