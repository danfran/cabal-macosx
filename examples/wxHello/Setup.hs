import Distribution.MacOSX
import Distribution.Simple
import System.Info (os)

main :: IO ()
main = defaultMainWithHooks $ addMacHook simpleUserHooks
  where addMacHook h =
          case os of
            -- is it OK to treat darwin as synonymous with MacOS X?
            "darwin" -> h { postBuild = appBundleBuildHook guiApps }
            _        -> h

guiApps :: Maybe [MacApp]
guiApps = Just [("WxHello", [MacInfoPlist "resources/WxHello.plist", 
                               MacIcon "resources/WxHello.icns"])
               ]
