module Distribution.MacOSX.Common where

import Data.List
import System.FilePath

-- | Mac OSX application information.
data MacApp = MacApp {
  -- | Application name.  This should be the name of the executable
  -- produced by Cabal's build stage.  The app bundle produced will be
  -- @dist\/build\//appName/.app@, and the executable /appName/ will
  -- be copied to @Contents\/MacOSX\/@ in the bundle.
  appName :: String,
  -- | Path to icon file, to be copied to @Contents\/Resources\/@ in
  -- the app bundle.  If omitted, no icon will be used.
  appIcon :: Maybe FilePath,
  -- | Path to /plist/ file ('property-list' of application metadata),
  -- to be copied to @Contents\/Info.plist@ in the app bundle.  If
  -- omitted, and if 'appIcon' is specified, a basic default plist
  -- will be used.
  appPlist :: Maybe FilePath,
  -- | Other resources to bundle in the application, e.g. image files,
  -- etc.  Each will be copied to @Contents\/Resources\/@, with the
  -- proviso that if the resource path begins with @resources\/@, it
  -- will go to a /relative/ subdirectory of @Contents\/Resources\/@.
  -- For example, @images/splash.png@ will be copied to
  -- @Contents\/Resources\/splash.png@, whereas
  -- @resources\/images\/splash.png@ will be copied to
  -- @Contents\/Resources\/resources\/images\/splash.png@.
  --
  -- Bundled resources may be referred to from your program relative
  -- to your executable's path (which may be computed, e.g., using
  -- Audrey Tang's FindBin package).
  resources :: [FilePath],
  -- | Other binaries to bundle in the application, e.g.  other
  -- executables from your project, or third-party programs.  Each
  -- will be copied to a relative sub-directory of
  -- @Contents\/Resources\/@ in the bundle.  For example,
  -- @\/usr\/bin\/ftp@ would be copied to
  -- @Contents\/Resources\/usr\/bin\/ftp@ in the app.
  --
  -- Like 'resources', bundled binaries may be referred to from your
  -- program relative to your executable's path (which may be
  -- computed, e.g., using Audrey Tang's FindBin package).
  otherBins :: [FilePath],
  -- | Controls inclusion of library dependencies for executable and
  -- 'otherBins'; see below.
  appDeps :: ChaseDeps
  } deriving (Eq, Show)

-- | Application bundles may carry their own copies of shared
-- libraries, which enables distribution of applications which 'just
-- work, out of the box' in the absence of static linking.  For
-- example, a wxHaskell app can include the wx library (and /its/
-- dependencies, recursively), meaning end users do not need to
-- install wxWidgets in order to use the app.
--
-- This data type controls this process: if dependency chasing is
-- activated, then the app's executable and any 'otherBins' are
-- examined for their dependencies, recursively (usually with some
-- exceptions - see below), the dependencies are copied into the app
-- bundle, and any references to each library are updated to point to
-- the copy.
--
-- (The process is transparent to the programmer, i.e. requires no
-- modification to code.  In case anyone is interested: @otool@ is
-- used to discover a binary's library dependencies; each library is
-- copied to a relative sub-directory of @Contents\/Frameworks\/@ in
-- the app bundle (e.g. @\/usr\/lib\/libFoo.a@ becomes
-- @Contents\/Frameworks\/usr\/lib\/libFoo.a@); finally,
-- @install_name_tool@ is used to update dependency references to
-- point to the new version.)
data ChaseDeps
  = -- | Do not include any dependencies - a sensible default if not
    -- distributing your app.
    DoNotChase
    -- | Include any libraries which the executable and 'otherBins'
    -- depend on, excluding a default set which we would expect to be
    -- present on any machine running the same version of OSX on which
    -- the executable was built.  (n.b.: Creation of application
    -- bundles which work transparently across different versions of
    -- OSX is currently beyond the scope of this package.)
  | ChaseWithDefaults
    -- | Include any libraries which the executable and 'otherBins'
    -- depend on, excluding a user-defined set.  If you specify an
    -- empty exclusion list, then /all/ dependencies will be included,
    -- recursively, including various OSX Frameworks; /this/
    -- /probably/ /isn't/ /ever/ /sensible/.  The intended use,
    -- rather, is to allow extension of the default list, which can be
    -- accessed via 'defaultExclusions'.
  | ChaseWith Exclusions
    deriving (Eq, Show)

-- | A list of exclusions to dependency chasing.  Any library whose
-- path contains any exclusion string /as a substring/ will be
-- excluded when chasing dependencies.
type Exclusions = [String]

-- | Default list of exclusions; excludes OSX standard frameworks,
-- libgcc, etc. - basically things which we would expect to be present
-- on any functioning OSX installation.
defaultExclusions :: Exclusions
defaultExclusions = 
  ["/System/Library/Frameworks/",
   "/libSystem.",
   "/libgcc_s.",
   "/libobjc."
  ]

-- | Compute item's path relative to app bundle root.
pathInApp :: MacApp -> FilePath -> FilePath
pathInApp app p
  | p == appName app = "Contents/MacOS" </> p
  | p `elem` otherBins app = "Contents/Resources" </> relP
  | p `elem` resources app = 
    let p' = if "resources/" `isPrefixOf` p then
               makeRelative "resources/" p
             else takeFileName p
    in "Contents/Resources" </> p'
  | otherwise = "Contents/Frameworks" </> relP
  where relP = makeRelative "/" p
