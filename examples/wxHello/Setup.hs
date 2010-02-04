-- Example Setup.hs for the wxHello app.

import Distribution.MacOSX
import Distribution.Simple
-- import System.Info (os)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps
       }

-- Checking by hand for OSX in Setup.hs no longer necessary.

-- main = defaultMainWithHooks $ addMacHook simpleUserHooks
--   where addMacHook h =
--           case os of
--             -- Here darwin == Mac OS X.  Is that OK?
--             "darwin" -> h { postBuild = appBundleBuildHook guiApps }
--             _        -> h

guiApps :: [MacApp]
guiApps = [MacApp "WxHello"
                  (Just "resources/WxHello.icns")
                  (Just "resources/WxHello.plist")
                  [] -- No other resources.
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
