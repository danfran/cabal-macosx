module Distribution.MacOSX.Common where

import System.FilePath

-- | A Mac application.
data MacApp = MacApp {
  -- | Application name.
  appName :: String,
  -- | Path to icon file to copy in..
  appIcon :: Maybe FilePath,
  -- | Path to plist file to copy to Contents/Info.plist.  May be
  -- omitted, but if an appIcon is specified, a default plist will be
  -- used.
  appPlist :: Maybe FilePath,
  -- | Other binaries to bundle in the application.
  otherBins :: [FilePath],
  -- | Chase dependencies for the app and bundled binaries?
  appDeps :: ChaseDeps
  } deriving (Eq, Show)

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
