module Distribution.MacOSX.Common

where

import System.FilePath

-- | A Mac application.
data MacApp = MacApp {
  -- | Application name.
  appName :: String,
  -- | Application resources - icon and plist.
  resources :: [AppResource],
  -- | Other binaries to bundle in the application.
  otherBins :: [FilePath],
  -- | Chase dependencies for the app and bundled binaries?
  appDeps :: ChaseDeps
  } deriving (Eq, Show)

-- | Application bundle customisations.
data AppResource =
  -- | Path to plist file to copy to Contents/Info.plist
  MacInfoPlist FilePath
  -- | Path to icon file (should also be referenced from plist file).
  | MacIcon FilePath
  deriving (Eq, Show)

-- | Either chase dependencies or don't; if you do, then allow
-- exclusions to be specified.
data ChaseDeps = ChaseWithDefaults
               | ChaseWith Exclusions
               | DoNotChase
                 deriving (Eq, Show)

-- | Exclusions to dependency chasing.
type Exclusions = [String]

-- | Compute item's path relative to app bundle root.
pathInApp :: MacApp -> FilePath -> FilePath
pathInApp app p
  | p == appName app = "Contents/MacOS" </> p
  | p `elem` otherBins app = "Contents/Resources" </> relP
  | otherwise = "Contents/Frameworks" </> relP
  where relP = makeRelative "/" p

foo :: MacApp
foo = MacApp "WxHello"
        [MacInfoPlist "resources/WxHello.plist", 
         MacIcon "resources/WxHello.icns"]
        ["/bin/ls"]
        ChaseWithDefaults
