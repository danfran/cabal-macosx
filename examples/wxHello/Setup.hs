-- Example Setup.hs for the wxHello app.

import Distribution.MacOSX
import Distribution.Simple
-- import System.Info (os)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "WxHello"
                  (Just "resources/WxHello.icns")
                  (Just "resources/WxHello.plist")
                  [] -- No other resources.
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
