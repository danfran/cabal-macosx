{- | Helpers for Mac OS X app distribution via Cabal.

'appBundleBuildHook' controls building application bundles for
whichever executables need them, possibly including customisations
(e.g. icon).

-}

module Distribution.MacOSX (
  MacApp(..),
  AppResource(..),
  ChaseDeps(..),
  Exclusions,
  defaultExclusions,
  appBundleBuildHook,
) where

import Control.Monad (forM_)
import Distribution.PackageDescription (PackageDescription(..),
                                        Executable(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags)
import System.Cmd
import System.FilePath
import System.Info (os)
import System.Directory (doesFileExist, copyFile, createDirectoryIfMissing)

import Distribution.MacOSX.Common
import Distribution.MacOSX.Dependencies

-- | Post-build hook for OS X application bundles.  Does nothing if
-- called on another O/S.
appBundleBuildHook ::
  [MacApp] -- ^ List of executables to build application bundles for;
           -- if empty, build for all executables in the package, with
           -- no icon or plist, and no dependency-chasing.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postInst'.
  -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleBuildHook apps _ _ pkg localb =
  case os of
    "darwin" -> forM_ apps' $ makeAppBundle localb
      where apps' = case apps of
                      [] -> map mkDefault $ executables pkg
                      xs -> xs
            mkDefault x = MacApp (exeName x) [] [] DoNotChase
    _ -> putStrLn "Not OS X, so not building an application bundle."

-- | Given a 'MacApp' in context, make an application bundle in the
-- build area.
makeAppBundle :: LocalBuildInfo -> MacApp -> IO ()
makeAppBundle localb app =
  do appPath <- createAppDir localb app
     includeDependencies appPath app
     customiseAppBundle appPath appN customs
        `catch` \err -> putStrLn ("Warning: could not customise bundle " ++
                                  "for " ++ appN ++ ": " ++ show err)
    where appN = appName app
          customs = resources app

-- | Create application bundle directory structure in build directory
-- and copy executable into it.  Returns path to newly created
-- directory.
createAppDir :: LocalBuildInfo -> MacApp -> IO FilePath
createAppDir localb app =
  do putStrLn $ "Creating application bundle directory " ++ appPath
     createDirectoryIfMissing False appPath
     createDirectoryIfMissing True  $ takeDirectory exeDest
     -- XXX always next line?  Or only if resources present?
     createDirectoryIfMissing True  $ appPath </> "Contents/Resources"
     putStrLn $ "Copying executable " ++ appName app ++ " into place"
     copyFile exeSrc exeDest
     return appPath
  where appPath = buildDir localb </> appName app <.> "app"
        exeDest = appPath </> pathInApp app (appName app)
        exeSrc = buildDir localb </> appName app </> appName app

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
