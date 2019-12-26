{-# LANGUAGE OverloadedStrings #-}
  {- | Cabal support for creating Mac OSX application bundles.

  GUI applications on Mac OSX should be run as application /bundles/;
  these wrap an executable in a particular directory structure which can
  also carry resources such as icons, program metadata, images, other
  binaries, and copies of shared libraries.

  This module provides a Cabal post-build hook for creating such
  application bundles, and controlling their contents.

  For more information about OSX application bundles, look for the
  /Bundle Programming Guide/ on the /Apple Developer Connection/
  website, <http://developer.apple.com/>.

  -}

module Distribution.MacOSX (
  appBundleBuildHook,
  appBundleInstallHook,
  appBundleCopyHook,
  makeAppBundle,
  MacApp(..),
  ChaseDeps(..),
  Exclusions,
  defaultExclusions
) where

import Control.Exception
import Prelude hiding ( catch )
import Control.Monad (forM_, when)
import Data.List ( isPrefixOf )
import Data.Text ( Text )
import System.Cmd (system)
import System.FilePath
import System.Directory (copyFile, createDirectoryIfMissing, getHomeDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.InstallDirs (bindir, prefix, CopyDest(NoCopyDest))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags, InstallFlags, CopyFlags, fromFlagOrDefault, installVerbosity, copyVerbosity)
import Distribution.Simple.Utils (installDirectoryContents, installExecutableFile)
import Distribution.System (Platform (..), OS (OSX))
import Distribution.Verbosity (normal, Verbosity)

import Distribution.MacOSX.Internal
import Distribution.MacOSX.AppBuildInfo
import Distribution.MacOSX.Common
import Distribution.MacOSX.Dependencies

-- | Post-build hook for OS X application bundles.  Does nothing if
-- called on another O/S.
appBundleBuildHook ::
  [MacApp] -- ^ List of applications to build; if empty, an
           -- application is built for each executable in the package,
           -- with no icon or plist, and no dependency-chasing.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postBuild'.
  -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleBuildHook apps _ _ pkg localb =
  if isMacOS localb
     then do let buildDirLbi = buildDir localb
             let macApps = getMacAppsForBuildableExecutors apps (executables pkg)
             forM_ macApps (makeAppBundle . createAppBuildInfo buildDirLbi)
     else putStrLn "Not OS X, so not building an application bundle."

-- | Post-install hook for OS X application bundles.  Copies the
-- application bundle (assuming you are also using the appBundleBuildHook)
-- to @$prefix/Applications@
-- Does nothing if called on another O/S.
appBundleInstallHook ::
  [MacApp] -- ^ List of applications to build; if empty, an
           -- application is built for each executable in the package,
           -- with no icon or plist, and no dependency-chasing.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postInstall'.
  -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleInstallHook apps _ iflags =
	appBundleInstallOrCopyHook apps
	    (fromFlagOrDefault normal (installVerbosity iflags))
{-# DEPRECATED appBundleInstallHook "Use appBundleCopyHook instead" #-}

-- | Post-copy hook for OS X application bundles.  Copies the
-- application bundle (assuming you are also using the appBundleBuildHook)
-- to @$prefix/Applications@
-- Does nothing if called on another O/S.
-- Use this instead of appBundleInstallHook (do not hook
-- both postInst and postCopy).
appBundleCopyHook ::
  [MacApp] -- ^ List of applications to build; if empty, an
           -- application is built for each executable in the package,
           -- with no icon or plist, and no dependency-chasing.
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postCopy'.
  -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleCopyHook apps _ cflags =
	appBundleInstallOrCopyHook apps
	    (fromFlagOrDefault normal (copyVerbosity cflags))

appBundleInstallOrCopyHook ::
  [MacApp] -- ^ List of applications to build; if empty, an
           -- application is built for each executable in the package,
           -- with no icon or plist, and no dependency-chasing.
  -> Verbosity
  -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleInstallOrCopyHook apps verbosity pkg localb = when (isMacOS localb) $ do
  libraryHaskell  <- flip fmap getHomeDirectory $ (</> "Library/Haskell")
  let standardPrefix = (libraryHaskell ++ "/") `isPrefixOf` prefix installDir
  let applicationsDir = if standardPrefix
                           then libraryHaskell    </> "Applications"
                           else prefix installDir </> "Applications"
  createDirectoryIfMissing False applicationsDir
  forM_ apps $ \app -> do
    let appInfo    = toAppBuildInfo localb app
        appPathSrc = abAppPath appInfo
        appPathTgt = applicationsDir </> takeFileName appPathSrc
        exe ap = ap </> pathInApp app (appName app)
    installDirectoryContents verbosity appPathSrc appPathTgt
    installExecutableFile    verbosity (exe appPathSrc) (exe appPathTgt)
    -- generate a tiny shell script for users who expect to run their
    -- applications from the command line with flags and all
    let script = if standardPrefix
                    then bundleScriptLibraryHaskell localb app
                    else bundleScriptElsewhere      localb app
        scriptFileSrc = buildDir localb   </> "_" ++ appName app <.> "sh"
        scriptFileTgt = bindir installDir </> appName app
    writeFile scriptFileSrc script
    installExecutableFile verbosity scriptFileSrc scriptFileTgt
  where
    installDir = absoluteInstallDirs pkg localb NoCopyDest

bundleScriptLibraryHaskell :: LocalBuildInfo -> MacApp -> String
bundleScriptLibraryHaskell localb app = unlines
  [ "#!/bin/bash"
  , "$HOME/Library/Haskell/Applications"
           </> takeFileName appPathSrc
           </> "Contents/MacOS" </> appName app ++ " \"$@\""
  ]
  where
    appInfo    = toAppBuildInfo localb app
    appPathSrc = abAppPath appInfo
 
bundleScriptElsewhere :: LocalBuildInfo -> MacApp -> String
bundleScriptElsewhere localb app = unlines
  [ "#!/bin/bash"
  , "MAX_DEPTH=256"
  , "COUNTER=0"
  , "ZERO=$0"
  , "STATUS=0"
  , ""
  , "# The counter is just a safeguard in case I'd done something silly"
  , "while [ $STATUS -eq 0 -a $COUNTER -lt $MAX_DEPTH ]; do"
  , "  COUNTER=$(($COUNTER+1))"
  , "  NZERO=`readlink $ZERO`; STATUS=$?"
  , "  if [ $STATUS -eq 0 ]; then"
  , "      # go to my parent dir"
  , "      pushd $(dirname $ZERO)  > /dev/null"
  , "      # now follow the symlink if at all"
  , "      pushd $(dirname $NZERO) > /dev/null"
  , "      ZERO=$PWD/$(basename $NZERO)"
  , "      popd > /dev/null"
  , "      popd > /dev/null"
  , "  fi"
  , "done"
  , "if [ $COUNTER -ge $MAX_DEPTH ]; then"
  , "  echo >&2 Urk! exceeded symlink depth of $MAX_DEPTH trying to dereference $0"
  , "  exit 1"
  , "fi"
  , "`dirname $ZERO`" </> "../Applications"
           </> takeFileName appPathSrc
           </> "Contents/MacOS" </> appName app ++ " \"$@\""
  ]
  where
    appInfo    = toAppBuildInfo localb app
    appPathSrc = abAppPath appInfo
 
isMacOS :: LocalBuildInfo -> Bool
isMacOS localb = case hostPlatform localb of
  Platform _ OSX -> True
  _ -> False

-- | Given a 'MacApp' in context, make an application bundle in the
-- build area. (for internal use only)
makeAppBundle :: AppBuildInfo -> IO ()
makeAppBundle appInfo@(AppBuildInfo appPath _ app) =
  do _ <- createAppDir appInfo
     maybeCopyPlist appPath app
     maybeCopyIcon appPath app
       `catch` \(SomeException err) ->
          putStrLn $ "Warning: could not set up icon for " ++ appName app ++ ": " ++ show err
     includeResources appPath app
     includeDependencies appPath app
     osxIncantations appPath app

-- | Create application bundle directory structure in build directory
-- and copy executable into it.  Returns path to newly created
-- directory.
createAppDir :: AppBuildInfo -> IO FilePath
createAppDir (AppBuildInfo appPath exeSrc app) =
  do putStrLn $ "Creating application bundle directory " ++ appPath
     createDirectoryIfMissing False appPath
     createDirectoryIfMissing True  $ takeDirectory exeDest
     createDirectoryIfMissing True  $ appPath </> "Contents/Resources"
     putStrLn $ "Copying executable " ++ appName app ++ " into place from " ++ exeSrc ++ " to " ++ exeDest
     copyFile exeSrc exeDest
     return appPath
  where exeDest = appPath </> pathInApp app (appName app)

-- | Include any external resources specified.
includeResources ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
includeResources appPath app = mapM_ includeResource $ resources app
    where includeResource :: FilePath -> IO ()
          includeResource p =
            do let pDest = appPath </> pathInApp app p
               putStrLn $ "Copying resource " ++ p ++ " to " ++ pDest
               createDirectoryIfMissing True $ takeDirectory pDest
               copyFile p $ pDest
               return ()

-- | If a plist has been specified, copy it into place.  If not, but
-- an icon has been specified, construct a default shell plist so the
-- icon is honoured.
maybeCopyPlist ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
maybeCopyPlist appPath app =
  case appPlist app of
    Just plPath -> do -- Explicit plist path, so copy it in and assume OK.
                      putStrLn $ "Copying " ++ plPath ++ " to " ++ plDest
                      copyFile plPath plDest
    Nothing -> case appIcon app of
                 Just icPath ->
                   do -- Need a plist to support icon; use default.
                     let pl  = T.replace "$program"  (T.pack (appName app)) plistTemplate
                         pl' = T.replace "$iconPath" (T.pack (takeFileName icPath)) pl
                     T.writeFile plDest pl'
                     return ()
                 Nothing -> return () -- No icon, no plist, nothing to do.
    where plDest = appPath </> "Contents/Info.plist"

-- | If an icon file has been specified, copy it into place.
maybeCopyIcon ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
maybeCopyIcon appPath app =
  case appIcon app of
    Just icPath ->
      do putStrLn $ "Copying " ++ icPath ++ " to app's icon"
         copyFile icPath $
                  appPath </> "Contents/Resources" </> takeFileName icPath
    Nothing -> return ()

-- | Default plist template, based on that in macosx-app from wx (but
-- with version stuff removed).
plistTemplate :: Text
plistTemplate = "\
    \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<!DOCTYPE plist SYSTEM \"file://localhost/System/Library/DTDs/PropertyList.dtd\">\n\
    \<plist version=\"0.9\">\n\
    \<dict>\n\
            \<key>CFBundleInfoDictionaryVersion</key>\n\
            \<string>6.0</string>\n\
            \<key>CFBundleIdentifier</key>\n\
            \<string>org.haskell.$program</string>\n\
            \<key>CFBundleDevelopmentRegion</key>\n\
            \<string>English</string>\n\
            \<key>CFBundleExecutable</key>\n\
            \<string>$program</string>\n\
            \<key>CFBundleIconFile</key>\n\
            \<string>$iconPath</string>\n\
            \<key>CFBundleName</key>\n\
            \<string>$program</string>\n\
            \<key>CFBundlePackageType</key>\n\
            \<string>APPL</string>\n\
            \<key>CFBundleSignature</key>\n\
            \<string>????</string>\n\
            \<key>CFBundleVersion</key>\n\
            \<string>1.0</string>\n\
            \<key>CFBundleShortVersionString</key>\n\
            \<string>1.0</string>\n\
            \<key>CFBundleGetInfoString</key>\n\
            \<string>$program, bundled by cabal-macosx</string>\n\
            \<key>LSRequiresCarbon</key>\n\
            \<true/>\n\
            \<key>CSResourcesFileMapped</key>\n\
            \<true/>\n\
    \</dict>\n\
    \</plist>"
