{-# LANGUAGE CPP #-}

import Control.Monad (foldM_, forM_)
import Data.Maybe ( fromMaybe )
import System.Cmd
import System.Exit
import System.Info (os)
import System.FilePath
import System.Directory ( doesFileExist, copyFile, removeFile, createDirectoryIfMissing )

import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

#ifndef WIN32
import System.Posix.Files (fileMode, getFileStatus, setFileMode,
                           ownerExecuteMode, groupExecuteMode, otherExecuteMode)
import Data.Bits ( (.|.) )
#endif

main :: IO ()
main = defaultMainWithHooks $ addMacHook simpleUserHooks
 where
  addMacHook h =
   case os of
    "darwin" -> h { postInst = appBundleHook } -- is it OK to treat darwin as synonymous with MacOS X?
    _        -> h

appBundleHook :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleHook _ _ pkg localb =
 forM_ exes $ \app ->
   do createAppBundle theBindir (buildDir localb </> app </> app)
      customiseAppBundle (appBundlePath theBindir app) app
        `catch` \err -> putStrLn $ "Warning: could not customise bundle for " ++ app ++ ": " ++ show err
      removeFile (theBindir </> app)
      createAppBundleWrapper theBindir app
 where
  theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest
  exes = fromMaybe (map exeName $ executables pkg) mRestrictTo

-- ----------------------------------------------------------------------
-- helper code for application bundles
-- ----------------------------------------------------------------------

-- | 'createAppBundle' @d p@ - creates an application bundle in @d@
--   for program @p@, assuming that @d@ already exists and is a directory.
--   Note that only the filename part of @p@ is used.
createAppBundle :: FilePath -> FilePath -> IO ()
createAppBundle dir p =
 do createDirectoryIfMissing False $ bundle
    createDirectoryIfMissing True  $ bundleBin
    createDirectoryIfMissing True  $ bundleRsrc
    copyFile p (bundleBin </> takeFileName p)
 where
  bundle     = appBundlePath dir p
  bundleBin  = bundle </> "Contents/MacOS"
  bundleRsrc = bundle </> "Contents/Resources"

-- | 'createAppBundleWrapper' @d p@ - creates a script in @d@ that calls
--   @p@ from the application bundle @d </> takeFileName p <.> "app"@
createAppBundleWrapper :: FilePath -> FilePath -> IO ()
createAppBundleWrapper bindir p =
  do writeFile scriptFile scriptTxt
     makeExecutable scriptFile
 where
  scriptFile = bindir </> takeFileName p
  scriptTxt = "`dirname $0`" </> appBundlePath "." p </> "Contents/MacOS" </> takeFileName p ++ " \"$@\""

appBundlePath :: FilePath -> FilePath -> FilePath
appBundlePath dir p = dir </> takeFileName p <.> "app"

-- ----------------------------------------------------------------------
-- utilities
-- ----------------------------------------------------------------------

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

-- | Put here IO actions needed to add any fancy things (eg icons)
--   you want to your application bundle.
customiseAppBundle :: FilePath -- ^ app bundle path
                   -> FilePath -- ^ full path to original binary
                   -> IO ()
customiseAppBundle bundleDir p =
 case takeFileName p of
  "geni" ->
    do hasRez <- doesFileExist "/Developer/Tools/Rez"
       if hasRez
          then do -- set the icon
                  copyFile "etc/macstuff/Info.plist" (bundleDir </> "Contents/Info.plist")
                  copyFile "etc/macstuff/wxmac.icns" (bundleDir </> "Contents/Resources/wxmac.icns")
                  -- no idea what this does
                  system ("/Developer/Tools/Rez -t APPL Carbon.r -o " ++ bundleDir </> "Contents/MacOS/geni")
                  writeFile (bundleDir </> "PkgInfo") "APPL????"
                  -- tell Finder about the icon
                  system ("/Developer/Tools/SetFile -a C " ++ bundleDir </> "Contents")
                  return ()
          else putStrLn "Developer Tools not found.  Too bad; no fancy icons for you."
  ""     -> return ()

-- | Put here the list of executables which contain a GUI.  If they all
--   contain a GUI (or you don't really care that much), just put Nothing
mRestrictTo :: Maybe [String]
mRestrictTo = Just ["geni"]
