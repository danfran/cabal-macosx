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
  { abAppPath    :: FilePath
  , abAppOrigExe :: FilePath
  , abApp        :: MacApp
  }

toAppBuildInfo :: LocalBuildInfo -> MacApp -> AppBuildInfo
toAppBuildInfo localb app = AppBuildInfo
  { abAppPath    = buildDir localb </> appName app <.> "app"
  , abAppOrigExe = buildDir localb </> appName app </> appName app
  , abApp        = app
  }
