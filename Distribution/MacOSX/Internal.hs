{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{- | Cabal support for creating Mac OSX application bundles.

GUI applications on Mac OSX should be run as application /bundles/;
these wrap an executable in a particular directory structure which can
also carry resources such as icons, program metadata, images, other
binaries, and copies of shared libraries.

This module provides internals to a Cabal post-build hook for creating such
application bundles, and controlling their contents.

For more information about OSX application bundles, look for the
/Bundle Programming Guide/ on the /Apple Developer Connection/
website, <http://developer.apple.com/>.

-}

module Distribution.MacOSX.Internal (
  getMacAppsForBuildableExecutors,
  osxIncantations
) where

#if MIN_VERSION_Cabal(2,0,0)
import Data.String (fromString)
import Distribution.Text (display)
#endif
import Prelude hiding ( catch )
import System.Cmd ( system )
import System.Exit
import System.FilePath
import Control.Monad (filterM)
import System.Directory (doesDirectoryExist)

import Distribution.PackageDescription (BuildInfo(..), Executable(..))
import Distribution.MacOSX.Common

-- | Filter or create new 'MacApp's that are associated by name to buildable 'Executable's.
getMacAppsForBuildableExecutors ::
  [MacApp] -- ^ List of 'MacApp's to filter if any.
  -> [Executable] -- ^ List of 'Executable's from .cabal.
  -> [MacApp] -- ^ Returned list of 'MacApp's that are associated by name to buildable 'Executable's.
getMacAppsForBuildableExecutors macApps executables =
  case macApps of
    [] -> map mkDefault buildables
    xs -> filter buildableApp xs
  where -- Make a default MacApp in absence of explicit from Setup.hs
#if MIN_VERSION_Cabal(2,0,0)
        mkDefault x = MacApp (display $ exeName x) Nothing Nothing [] [] DoNotChase
#else
        mkDefault x = MacApp (exeName x) Nothing Nothing [] [] DoNotChase
#endif

        -- Check if a MacApp is in that list of buildable executables.
        buildableApp :: MacApp -> Bool
#if MIN_VERSION_Cabal(2,0,0)
        buildableApp app = any (\e -> exeName e == fromString (appName app)) buildables
#else
        buildableApp app = any (\e -> exeName e == appName app) buildables
#endif

        -- List of buildable executables from .cabal file.
        buildables :: [Executable]
        buildables = filter (buildable . buildInfo) executables

-- | Perform various magical OS X incantations for turning the app
-- directory into a bundle proper.
osxIncantations ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
osxIncantations appPath app =
  do writeFile (appPath </> "PkgInfo") "APPL????"
     return ()
