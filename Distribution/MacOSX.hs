{-# LANGUAGE CPP #-}

{- | Helpers for Mac OS X app distribution via Cabal.

'appBundleBuildHook' controls building application bundles for
whichever executables need them, possibly including customisations
(e.g. icon).

-}

module Distribution.MacOSX (
  MacApp(..),
  AppResource(..),
  ChaseDeps(..),
  appBundleBuildHook
) where

import Control.Monad (forM_)
import System.Cmd
import System.FilePath
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing)
import Distribution.PackageDescription (PackageDescription(..),
                                        Executable(..))
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))

-- | A Mac application.
data MacApp = MacApp {
  -- | Application name.
  appName :: String,
  -- | Application resources - icon and plist.
  resources :: [AppResource],
  -- | Chase dependencies for the app?
  appDeps :: ChaseDeps,
  -- | Other binaries to bundle in the application.
  otherBins :: [(FilePath, ChaseDeps)]
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
data ChaseDeps = ChaseDeps [String]
               | NoChaseDeps
                 deriving (Eq, Show)

-- | Post-build hook for OS X application bundles.
appBundleBuildHook ::
  [MacApp] -- ^ List of executables to build application bundles for;
           -- if empty, build for all executables in the package, with
           -- default customisations and dependency-chasing.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postInst'.
  -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleBuildHook apps _ _ pkg localb =
  forM_ apps' $ makeAppBundle localb
    where apps' = case apps of
            [] -> map mkDefaultApp $ executables pkg
            xs -> xs
          mkDefaultApp x = MacApp (exeName x) [] (ChaseDeps []) []

-- | Given a 'MacApp' in context, make an application bundle in the
-- build area.
makeAppBundle :: LocalBuildInfo -> MacApp -> IO ()
makeAppBundle localb appExe =
  do createAppBundle (buildDir localb) (appParent </> app)
     customiseAppBundle bundlePath app customs
        `catch` \err -> putStrLn ("Warning: could not customise bundle " ++
                                  "for " ++ app ++ ": " ++ show err)
    where bundlePath = appBundlePath (buildDir localb) app
          appParent = buildDir localb </> app
          app = appName appExe
          customs = resources appExe

-- ----------------------------------------------------------------------
-- helper code for application bundles
-- ----------------------------------------------------------------------

-- | 'createAppBundle' @d p@ - creates an application bundle in @d@
-- for program @p@, assuming that @d@ already exists and is a
-- directory.  Note that only the filename part of @p@ is used.
createAppBundle :: FilePath -> FilePath -> IO ()
createAppBundle dir p =
  do createDirectoryIfMissing False bundle
     createDirectoryIfMissing True  bundleBin
     createDirectoryIfMissing True  bundleRsrc
     copyFile p (bundleBin </> takeFileName p)
    where bundle     = appBundlePath dir p
          bundleBin  = bundle </> "Contents/MacOS"
          bundleRsrc = bundle </> "Contents/Resources"

-- | 'appBundlePath' @d p@ - compute path to application bundle in @d@
-- for program @p@.
appBundlePath :: FilePath -> FilePath -> FilePath
appBundlePath dir p = dir </> takeFileName p <.> "app"

-- ----------------------------------------------------------------------
-- customisations
-- ----------------------------------------------------------------------

-- | Put here IO actions needed to add any fancy things (eg icons) you
-- want to your application bundle.
customiseAppBundle :: FilePath -- ^ app bundle path
                   -> FilePath -- ^ full path to original binary XXX
                   -> [AppResource]
                   -> IO ()
customiseAppBundle bundleDir p cs =
  do hasRez <- doesFileExist "/Developer/Tools/Rez"
     if hasRez
       then mapM_ (copyResource bundleDir p) cs
       else putStrLn $ "Developer Tools not found.  Too bad; " ++
              "no fancy icons for you."

-- XXX We currently basically demand that the user supply an
-- Info.plist if they want an icon - but doesn't macosx-app
-- automatically create one if necessary?  I _think_ so - in which
-- case it would be nice to duplicate that here.

-- | @copyResource b n c@ - copies custom resource @c@ for application
-- @n@ into place in bundle @b@.
copyResource :: FilePath -> FilePath -> AppResource -> IO ()
copyResource bundleDir _ (MacInfoPlist f) =
  do putStrLn $ "Copying " ++ f ++ " to Info.plist in bundle."
     copyFile f (bundleDir </> "Contents/Info.plist")
copyResource bundleDir app (MacIcon f) =
  do putStrLn $ "Setting " ++ f ++ " to bundle's icon."
     copyFile f (bundleDir </> "Contents/Resources" </> takeFileName f)
     -- Now some Carbon-related voodoo.
     system ("/Developer/Tools/Rez Carbon.r -o " ++ bundleDir </>
             "Contents/MacOS" </> app)
     writeFile (bundleDir </> "PkgInfo") "APPL????"
     -- tell Finder about the icon
     system ("/Developer/Tools/SetFile -a C " ++ bundleDir </> "Contents")
     return ()
