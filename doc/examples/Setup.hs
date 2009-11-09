-- Example Setup.hs file showing simple use.

import Distribution.MacOSX (appBundleHook)
import Distribution.Simple
import System.Info (os)

main :: IO ()
main = defaultMainWithHooks $ addMacHook simpleUserHooks
  where addMacHook h =
          case os of
            -- XXX Is it OK to treat darwin as synonymous with MacOS X?
            "darwin" -> h { postInst = appBundleHook guiApps }
            _        -> h

-- | appBundleHook's first argument is a Maybe [String] containing
-- names of executables (as found in your .cabal file) for which an
-- app bundle should be built.  If it's Nothing, then an app bundle
-- will be built for _all_ your executables (so in most simple cases,
-- this is just what you want).  (And of course Just [] means don't
-- create app bundles for any of them.)
guiApps :: Maybe [String]
guiApps = Just ["MyGuiApp1", "MyGuiApp2"]
