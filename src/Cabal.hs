{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cabal
    ( getExecutableOptions
    ) where

import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Monoid (mempty)

import Control.Monad.State (MonadState, State, execState, modify)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..),
                                        Executable(exeName))
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB))
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Setup (ConfigFlags(..), toFlag)
import Distribution.Version (Version(..))
import Language.Haskell.Extension (Language(Haskell98))
import qualified Distribution.PackageDescription

newtype Cabal a = Cabal (State GhcOptions a)
    deriving (Functor, Monad, MonadState GhcOptions)

class HasBuildInfo a where
    buildInfo :: a -> BuildInfo

instance HasBuildInfo Executable where
    buildInfo = Distribution.PackageDescription.buildInfo

execCabal :: Cabal a -> GhcOptions
execCabal (Cabal f) = execState f mempty

addExtensions :: HasBuildInfo a => a -> Cabal ()
addExtensions bi = modify $ \f -> f { ghcOptExtensions = defaultExtensions $ buildInfo bi }

addSourceDirs :: HasBuildInfo a => a -> Cabal ()
addSourceDirs bi = modify $ \f -> f { ghcOptSourcePath = hsSourceDirs $ buildInfo bi }

addGhcOptions :: HasBuildInfo a => a -> Cabal ()
addGhcOptions bi = modify $ \f -> f { ghcOptExtra = extraOpts }
  where
    extraOpts = go $ options $ buildInfo bi
    go ((GHC, opts): xs) = opts ++ go xs
    go (_:xs) = go xs
    go [] = []

addLanguage :: HasBuildInfo a => a -> Cabal ()
addLanguage bi = modify $ \f -> f { ghcOptLanguage = langFlag }
  where
    lang = fromMaybe Haskell98 $ defaultLanguage $ buildInfo bi
    langFlag = toFlag lang

addPackageDbs :: LocalBuildInfo -> Cabal ()
addPackageDbs lbi = modify $ \f -> f { ghcOptPackageDBs = dbs }
  where
    dbs = GlobalPackageDB : (catMaybes $ configPackageDBs $ configFlags lbi)

addExecutableOptions :: String -> LocalBuildInfo -> Cabal ()
addExecutableOptions name lbi = do
    addPackageDbs lbi
    addLanguage exec
    addExtensions exec
    addSourceDirs exec
    addGhcOptions exec
  where
    notFound = error $ "Executable " ++ name ++ " not found"
    mbExec = listToMaybe $ filter ((name ==) . exeName) $ executables descr
    exec = fromMaybe notFound mbExec
    descr = localPkgDescr lbi

getExecutableOptions :: String -> IO [String]
getExecutableOptions name = do
    buildConfig <- getPersistBuildConfig "dist"
    return $ renderGhcOptions version $ execCabal $
        addExecutableOptions name buildConfig
  where
    version = Version [7, 6, 1] []
