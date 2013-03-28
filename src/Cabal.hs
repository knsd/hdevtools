{-# LANGUAGE RecordWildCards #-}

module Cabal
    ( getExecutableOptions
    ) where

import Data.Maybe (listToMaybe, maybeToList, fromMaybe, catMaybes)
import Data.Monoid (mempty)

import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription (PackageDescription(..), BuildInfo(..),
                                        Executable(exeName))
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB))
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.Text (Text, display)
import Distribution.Version (Version(..))
import Language.Haskell.Extension (Language, Extension)
import qualified Distribution.PackageDescription

class HasBuildInfo a where
    buildInfo :: a -> BuildInfo

instance HasBuildInfo Executable where
    buildInfo = Distribution.PackageDescription.buildInfo

getExtensions :: HasBuildInfo a => a -> [Extension]
getExtensions = defaultExtensions . buildInfo

getSourceDirs :: HasBuildInfo a => a -> [FilePath]
getSourceDirs = hsSourceDirs . buildInfo

getGhcOptions :: HasBuildInfo a => a -> [String]
getGhcOptions = go . options . buildInfo
  where
    go ((GHC, opts): xs) = opts ++ go xs
    go (_:xs) = go xs
    go [] = []

getLanguage :: HasBuildInfo a => a -> Maybe Language
getLanguage = defaultLanguage . buildInfo

getOptions :: HasBuildInfo a => a -> [String]
getOptions x = concat [extensions, sourceDirs, ghcOptions, language]
  where
    mkExt :: Text a => a -> String
    mkExt = ("-X" ++) . display
    extensions = map mkExt $ getExtensions x
    language = map mkExt $ maybeToList $ getLanguage x
    sourceDirs = map ("-i" ++) $ getSourceDirs x
    ghcOptions = getGhcOptions x

executableOptions :: String -> PackageDescription ->[String]
executableOptions name descr = getOptions executable
  where
    executable = fromMaybe (error notFound) $ listToMaybe $
        filter ((name ==) . exeName) $ executables descr
    notFound = "Executable " ++ name ++ " not found"

getExecutableOptions :: String -> IO [String]
getExecutableOptions name = do
    buildConfig <- getPersistBuildConfig "dist"
    let flags = configFlags buildConfig
    let dbs = GlobalPackageDB : (catMaybes $ configPackageDBs flags)
    let ghcOptions = mempty { ghcOptPackageDBs = dbs }
    let rendered = renderGhcOptions version ghcOptions
    return $ rendered ++ (executableOptions name $ localPkgDescr buildConfig)
  where
    version = Version [7, 6, 1] []
