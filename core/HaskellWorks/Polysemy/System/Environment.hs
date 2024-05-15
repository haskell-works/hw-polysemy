module HaskellWorks.Polysemy.System.Environment
  ( getArgs,
    getProgName,
    executablePath,
    getExecutablePath,
    getEnv,
    lookupEnv,
    setEnv,
    unsetEnv,
    getEnvironment,
  ) where

import qualified Control.Exception             as CE
import           HaskellWorks.Polysemy.Prelude
import           Polysemy
import           Polysemy.Error
import qualified System.Environment            as IO

getArgs :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Sem r [String]
getArgs = withFrozenCallStack $
  embed IO.getArgs

getProgName :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Sem r String
getProgName = withFrozenCallStack $
  embed IO.getProgName

executablePath :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Maybe (Sem r (Maybe FilePath))
executablePath = withFrozenCallStack $
  embed <$> IO.executablePath

getExecutablePath :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Sem r String
getExecutablePath = withFrozenCallStack $
  embed IO.getExecutablePath

getEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Member (Error IOException) r
  => String
  -> Sem r String
getEnv name = withFrozenCallStack $ do
  r <- embed $ CE.try @IOException $ IO.getEnv name
  fromEither r

lookupEnv :: ()
  => HasCallStack
  => Member (Embed IO) r
  => String
  -> Sem r (Maybe String)
lookupEnv name = withFrozenCallStack $
  embed $ IO.lookupEnv name

setEnv :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => String
  -> String
  -> Sem r ()
setEnv name value = withFrozenCallStack $ do
  r <- embed $ CE.try @IOException $ IO.setEnv name value
  fromEither r

unsetEnv :: ()
  => HasCallStack
  => Member (Error IOException) r
  => Member (Embed IO) r
  => String
  -> Sem r ()
unsetEnv name = withFrozenCallStack $ do
  r <- embed $ CE.try @IOException $ IO.unsetEnv name
  fromEither r

getEnvironment :: ()
  => HasCallStack
  => Member (Embed IO) r
  => Sem r [(String, String)]
getEnvironment = withFrozenCallStack $
  embed IO.getEnvironment
