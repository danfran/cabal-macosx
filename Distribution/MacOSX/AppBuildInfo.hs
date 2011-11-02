module Distribution.MacOSX.AppBuildInfo where

import Control.Monad (forM_, when)
import Data.String.Utils (replace)
import Distribution.PackageDescription (PackageDescription(..),
                                        Executable(..))
import Distribution.Simple
import Distribution.Simple.InstallDirs (bindir, prefix, CopyDest(NoCopyDest))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags, InstallFlags,
                                  fromFlagOrDefault, installVerbosity
                                 )
import Distribution.Simple.Utils (installDirectoryContents, installExecutableFile)
import Distribution.Verbosity (normal)
import System.Cmd (system)
import System.FilePath
import System.Info (os)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit

import Distribution.MacOSX.Common
import Distribution.MacOSX.Dependencies

-- | Information needed to build a bundle
--
--   This exists to make it possible to have a standalone
--   macosx-app executable without it necessarily having to
--   know a lot of Cabal internals
data AppBuildInfo = AppBuildInfo
  { appPath    :: FilePath
  , appOrigExe :: FilePath
  , app        :: MacApp
  }

toAppBuildInfo :: LocalBuildInfo -> MacApp -> AppBuildInfo
toAppBuildInfo localb app_ = AppBuildInfo
  { appPath    = buildDir localb </> appName app_ <.> "app"
  , appOrigExe = buildDir localb </> appName app_ </> appName app_
  , app        = app_
  }
