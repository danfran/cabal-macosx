-- Example Setup.hs for the wxHello app, which only attempts to use
-- cabal-macosx machinery if on OSX.

#if darwin_HOST_OS == 1
import Distribution.MacOSX
#endif
import Distribution.Simple
-- import System.Info (os)

main :: IO ()
#if darwin_HOST_OS == 1
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps
       }

guiApps :: [MacApp]
guiApps = [MacApp "WxHello"
                  (Just "resources/WxHello.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  [] -- No other resources.
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
#else
main = defaultMain
#endif
