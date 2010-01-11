-- Example Setup.hs for the wxHello app.

import Distribution.MacOSX
import Distribution.Simple
import System.Info (os)

main :: IO ()
main = defaultMainWithHooks $ addMacHook simpleUserHooks
  where addMacHook h =
          case os of
            -- Here darwin == Mac OS X.  Is that OK?
            "darwin" -> h { postBuild = appBundleBuildHook guiApps }
            _        -> h

-- | appBundleHook's first argument is a Maybe [MacApp] containing
-- names of executables (as found in your .cabal file) for which an
-- app bundle should be built, and their and custom resources, if any.
-- If it's Nothing, then an app bundle will be built for _all_ your
-- executables (so in most simple cases, this is just what you want).
-- (And of course Just [] means don't create app bundles for any of
-- them.)
--
-- In future, MacApp values will probably carry other customisation
-- information, such as paths of shared libraries to be baked into the
-- bundle.
guiApps :: Maybe [MacApp]
guiApps = Just [("WxHello", [MacInfoPlist "resources/WxHello.plist", 
                             MacIcon "resources/WxHello.icns"])
               ]
