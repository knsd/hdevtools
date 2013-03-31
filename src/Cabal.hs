{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cabal
    ( Component(..)
    , getComponentOptions
    ) where

import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Monoid (mempty)

import Control.Monad.State (MonadState, State, execState, modify)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..),
                                        Executable(exeName), Library(libBuildInfo))
import Distribution.Simple.Compiler (Compiler(compilerExtensions), PackageDB(GlobalPackageDB))
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Setup (ConfigFlags(..), toFlag)
import Distribution.Version (Version(..))
import Language.Haskell.Extension (Language(Haskell98))
import qualified Distribution.PackageDescription

data Any c = forall a. c a => Lift a

newtype Cabal a = Cabal (State GhcOptions a)
    deriving (Functor, Monad, MonadState GhcOptions)

type Name = String

data Component = ComponentLibrary
               | ComponentExecutable Name

class HasBuildInfo a where
    buildInfo :: a -> BuildInfo

instance HasBuildInfo Executable where
    buildInfo = Distribution.PackageDescription.buildInfo

instance HasBuildInfo Library where
    buildInfo = libBuildInfo

instance HasBuildInfo (Any HasBuildInfo) where
    buildInfo (Lift x) = buildInfo x

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

addExtensionMap :: LocalBuildInfo -> Cabal ()
addExtensionMap lbi = modify $ \f -> f { ghcOptExtensionMap = compilerExtensions $ compiler lbi }

addComponentOptions :: LocalBuildInfo -> (LocalBuildInfo -> Any HasBuildInfo) -> Cabal ()
addComponentOptions lbi getComp = let comp = getComp lbi in do
    addPackageDbs lbi
    addExtensionMap lbi
    addLanguage comp
    addExtensions comp
    addSourceDirs comp
    addGhcOptions comp

getComponent :: Component -> LocalBuildInfo -> Any HasBuildInfo
getComponent ComponentLibrary = Lift . fromMaybe notFound . library . localPkgDescr
  where
    notFound = error "Library not found"
getComponent (ComponentExecutable name) = Lift .  fromMaybe notFound . listToMaybe .
    filter ((name ==) . exeName) . executables . localPkgDescr
  where
    notFound = error $ "Executable " ++ name ++ " not found"

getComponentOptions :: Component -> IO [String]
getComponentOptions component = do
    buildConfig <- getPersistBuildConfig "dist"
    return $ renderGhcOptions version $ execCabal $
        addComponentOptions buildConfig $ getComponent component
  where
    version = Version [7, 6, 1] []
