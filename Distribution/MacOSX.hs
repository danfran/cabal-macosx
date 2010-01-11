{-# LANGUAGE CPP #-}

{- | Helpers for Mac OS X app distribution via Cabal.

'appBundleBuildHook' controls building application bundles for
whichever executables need them, possibly including customisations
(e.g. icon).

-}

module Distribution.MacOSX (
  MacApp,
  MacCustom(..),
  appBundleBuildHook
) where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.Cmd
import System.FilePath
import System.Directory (doesFileExist, copyFile, removeFile,
                         createDirectoryIfMissing)

import Distribution.PackageDescription (PackageDescription(..),
                                        Executable(..))
import Distribution.Simple.InstallDirs (bindir)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags, CopyDest(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs,
                                           LocalBuildInfo(..))

-- XXX What's the purpose of the WIN32 stuff?  We're doing
-- Mac-specific processing here, aren't we?  Is this to do with
-- cross-compilation or some such exotica?  :-)
#ifndef WIN32
import System.Posix.Files (fileMode, getFileStatus, setFileMode,
                           ownerExecuteMode, groupExecuteMode,
                           otherExecuteMode)
import Data.Bits ( (.|.) )
#endif

-- | Executables for which an application bundle should built,
-- identified by executable name (as specified in the relevant file),
-- along with their customisations.
type MacApp = (String, [MacCustom])

-- | Application bundle customisations.
data MacCustom =
  -- | Path to plist file to copy to Contents/Info.plist
  MacInfoPlist FilePath
  -- | Path to icon file (should also be referenced from plist file).
  | MacIcon FilePath
  deriving (Eq, Show)

-- | Post-build hook for OS X application bundles.
appBundleBuildHook ::
  Maybe [MacApp] -- ^ List of executables to build application bundles
                 -- for; @Nothing@ means build for all, with no
                 -- customisations.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postInst'.
  -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleBuildHook appExes _ _ pkg localb =
  forM_ exes $ \app -> makeAppBundle localb app pkg
    where exes = case appExes of
            Just x -> x
            Nothing -> map (\x -> (exeName x, [])) $ executables pkg

-- | Given a 'MacApp' in context, make an application bundle in the
-- build area.
makeAppBundle :: LocalBuildInfo -> MacApp -> PackageDescription -> IO ()
makeAppBundle localb (app, customs) pkg =
  do createAppBundle (buildDir localb) (appParent </> app)
     customiseAppBundle bundlePath app customs
        `catch` \err -> putStrLn $ ("Warning: could not customise bundle " ++
                                    "for " ++ app ++ ": " ++ show err)
     -- removeFile (theBindir </> app)
     -- createAppBundleWrapper theBindir app
    where bundlePath = appBundlePath (buildDir localb) app
          appParent = buildDir localb </> app
          -- theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest

-- ----------------------------------------------------------------------
-- helper code for application bundles
-- ----------------------------------------------------------------------

-- | 'createAppBundle' @d p@ - creates an application bundle in @d@
-- for program @p@, assuming that @d@ already exists and is a
-- directory.  Note that only the filename part of @p@ is used.
createAppBundle :: FilePath -> FilePath -> IO ()
createAppBundle dir p =
  do createDirectoryIfMissing False $ bundle
     createDirectoryIfMissing True  $ bundleBin
     createDirectoryIfMissing True  $ bundleRsrc
     copyFile p (bundleBin </> takeFileName p)
    where bundle     = appBundlePath dir p
          bundleBin  = bundle </> "Contents/MacOS"
          bundleRsrc = bundle </> "Contents/Resources"

-- | 'createAppBundleWrapper' @d p@ - creates a script in @d@ that
-- calls @p@ from the application bundle. 
createAppBundleWrapper :: FilePath -> FilePath -> IO ()
createAppBundleWrapper binDir p =
  do writeFile scriptFile scriptTxt
     makeExecutable scriptFile
   where scriptFile = binDir </> takeFileName p
         scriptTxt = "`dirname $0`" </> appBundlePath "." p </>
                     "Contents/MacOS" </> takeFileName p ++ " \"$@\""

-- | 'appBundlePath' @d p@ - compute path to application bundle in @d@
-- for program @p@.
appBundlePath :: FilePath -> FilePath -> FilePath
appBundlePath dir p = dir </> takeFileName p <.> "app"

-- ----------------------------------------------------------------------
-- utilities
-- ----------------------------------------------------------------------

-- | Make a file executable by all.
makeExecutable :: FilePath -> IO ()
#ifdef WIN32
makeExecutable = const (return ())
#else
makeExecutable f =
  do st <- getFileStatus f
     let m  = fileMode st
         m2 = m .|. ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode
     setFileMode f m2
#endif

-- ----------------------------------------------------------------------
-- customisations
-- ----------------------------------------------------------------------

-- | Put here IO actions needed to add any fancy things (eg icons) you
-- want to your application bundle.
customiseAppBundle :: FilePath -- ^ app bundle path
                   -> FilePath -- ^ full path to original binary XXX
                   -> [MacCustom]
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
copyResource :: FilePath -> FilePath -> MacCustom -> IO ()
copyResource bundleDir _ (MacInfoPlist f) =
  do putStrLn $ "Copying " ++ f ++ " to Info.plist in bundle."
     copyFile f (bundleDir </> "Contents/Info.plist")
copyResource bundleDir appName (MacIcon f) =
  do putStrLn $ "Setting " ++ f ++ " to bundle's icon."
     copyFile f (bundleDir </> "Contents/Resources" </> takeFileName f)
     -- Now some Carbon-related voodoo.
     system ("/Developer/Tools/Rez Carbon.r -o " ++ bundleDir </>
             "Contents/MacOS" </> appName)
     writeFile (bundleDir </> "PkgInfo") "APPL????"
     -- tell Finder about the icon
     system ("/Developer/Tools/SetFile -a C " ++ bundleDir </> "Contents")
     return ()



