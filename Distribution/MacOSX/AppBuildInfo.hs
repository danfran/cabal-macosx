-- | Information used to help create an application bundle
module Distribution.MacOSX.AppBuildInfo where

import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
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

-- | @toAppBuildInfo l m@ returns information for an application bundle
--   within the @l@ build directory
toAppBuildInfo :: LocalBuildInfo -> MacApp -> AppBuildInfo
toAppBuildInfo localb app = AppBuildInfo
  { abAppPath    = buildDir localb </> appName app <.> "app"
  , abAppOrigExe = buildDir localb </> appName app </> appName app
  , abApp        = app
  }
