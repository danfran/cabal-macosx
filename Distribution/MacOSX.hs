{-# LANGUAGE CPP #-}

{- | Helpers for Mac OS X app distribution via Cabal.

-}

module Distribution.MacOSX (
  appBundleHook
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
import Distribution.Simple (Args)
import Distribution.Simple.Setup (InstallFlags, CopyDest(..))
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

-- appExes - put here the list of executables which contain a GUI.
-- If they all contain a GUI (or you don't really care that much),
-- just put Nothing.
appBundleHook :: Maybe [String] -> Args -> InstallFlags ->
                 PackageDescription -> LocalBuildInfo -> IO ()
appBundleHook appExes _ _ pkg localb =
  forM_ exes $ \app -> makeAppBundle localb app pkg
    where exes = fromMaybe (map exeName $ executables pkg) appExes
                       
makeAppBundle :: LocalBuildInfo -> FilePath -> PackageDescription -> IO()
makeAppBundle localb app pkg = 
  do createAppBundle theBindir (buildDir localb </> app </> app)
     customiseAppBundle (appBundlePath theBindir app) app
        `catch` \err -> putStrLn $ ("Warning: could not customise bundle " ++
                                    "for " ++ app ++ ": " ++ show err)
     removeFile (theBindir </> app)
     createAppBundleWrapper theBindir app
    where theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest

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
                   -> FilePath -- ^ full path to original binary
                   -> IO ()
customiseAppBundle bundleDir p =
  case takeFileName p of
    "geni" ->
      do hasRez <- doesFileExist "/Developer/Tools/Rez"
         if hasRez
           then setIcon bundleDir "geni"
           else putStrLn $ "Developer Tools not found.  Too bad; " ++
                  "no fancy icons for you."
    _     -> return ()

-- | Set the icon.
setIcon :: FilePath -> FilePath -> IO ()
setIcon bundleDir appName =
  do copyFile "etc/macstuff/Info.plist" (bundleDir </>
                                         "Contents/Info.plist")
     copyFile "etc/macstuff/wxmac.icns" (bundleDir </>
                                         "Contents/Resources/wxmac.icns")
     -- no idea what this does
     system ("/Developer/Tools/Rez -t APPL Carbon.r -o " ++
             bundleDir </> "Contents/MacOS" </> appName)
     writeFile (bundleDir </> "PkgInfo") "APPL????"
     -- tell Finder about the icon
     system ("/Developer/Tools/SetFile -a C " ++ bundleDir </> "Contents")
     return ()