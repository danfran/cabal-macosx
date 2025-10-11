{-# LANGUAGE CPP #-}
-- | Information used to help create an application bundle
module Distribution.MacOSX.AppBuildInfo where

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), buildDir)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (SymbolicPathX, getSymbolicPath)
#endif
import System.FilePath

import Distribution.MacOSX.Common

-- | Information needed to build a bundle
--
--   This exists to make it possible to have a standalone
--   macosx-app executable without it necessarily having to
--   know a lot of Cabal internals
data AppBuildInfo = AppBuildInfo
  { -- | Location of the application bundle being built
    abAppPath    :: FilePath
    -- | Location of the original executable that was built
  , abAppOrigExe :: FilePath
    -- |
  , abApp        :: MacApp
  }

#if MIN_VERSION_Cabal(3,14,0)
getSymbolicPathCompat :: SymbolicPathX allowAbsolute from to -> FilePath
getSymbolicPathCompat = getSymbolicPath
#else
getSymbolicPathCompat :: FilePath -> FilePath
getSymbolicPathCompat = id
#endif

-- | @toAppBuildInfo l m@ returns information for an application bundle
--   within the @l@ build directory
toAppBuildInfo :: LocalBuildInfo -> MacApp -> AppBuildInfo
toAppBuildInfo localb app = createAppBuildInfo (getSymbolicPathCompat $ buildDir localb) app

-- | @createAppBuildInfo d m@ returns information for an application bundle
--   within the @d@ build directory from LocalBuildInfo
createAppBuildInfo :: FilePath -> MacApp -> AppBuildInfo
createAppBuildInfo buildDirLocalBuildInfo app = AppBuildInfo
  { abAppPath    = buildDirLocalBuildInfo </> appName app <.> "app"
  , abAppOrigExe = buildDirLocalBuildInfo </> appName app </> appName app
  , abApp        = app
  }
