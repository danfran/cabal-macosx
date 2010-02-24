-- Example Setup.hs for the gtkHello app.

import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "GtkHello"
                  (Just "resources/GtkHello.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  [] -- No other resources.
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
