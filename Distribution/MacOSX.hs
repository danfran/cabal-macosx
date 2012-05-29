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
  appBundleBuildHook, appBundleInstallHook,
  makeAppBundle,
  MacApp(..),
  ChaseDeps(..),
  Exclusions,
  defaultExclusions
) where

import Control.Exception
import Prelude hiding ( catch )
import Control.Monad (forM_, when, filterM)
import Data.List ( isPrefixOf )
import Data.String.Utils (replace)
import Distribution.PackageDescription (BuildInfo(..),
                                        Executable(..),
                                        PackageDescription(..))
import Distribution.Simple
import Distribution.Simple.InstallDirs (bindir, prefix, CopyDest(NoCopyDest))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags, InstallFlags,
                                  fromFlagOrDefault, installVerbosity
                                 )
import Distribution.Simple.Utils (installDirectoryContents, installExecutableFile)
import Distribution.Verbosity (normal)
import System.Cmd (system)
import System.FilePath
import System.Info (os)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist,
                         getHomeDirectory)
import System.Exit

import Distribution.MacOSX.AppBuildInfo
import Distribution.MacOSX.Common
import Distribution.MacOSX.Dependencies

-- | Post-build hook for OS X application bundles.  Does nothing if
-- called on another O/S.
appBundleBuildHook ::
  [MacApp] -- ^ List of applications to build; if empty, an
           -- application is built for each executable in the package,
           -- with no icon or plist, and no dependency-chasing.
           -- In any case, apps are only built for executables whose
           -- buildable flag is True (which they are by default in
           -- Cabal).
  -> Args -- ^ All other parameters as per
          -- 'Distribution.Simple.postBuild'.
  -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleBuildHook apps _ _ pkg localb =
  if isMacOS
     then case apps' of
            [] -> putStrLn "No buildable MacApps, so making no bundles."
            _  -> forM_ apps' $ makeAppBundle . toAppBuildInfo localb
     else putStrLn "Not OS X, so not building an application bundle."
  where
    apps' = case apps of
              [] -> map mkDefault buildables
              xs -> filter buildableApp xs
    -- List of buildable executables from .cabal file.
    buildables :: [Executable]
    buildables = filter (buildable . buildInfo) $ executables pkg
    -- Check if a MacApp is in that list of buildable executables.
    buildableApp :: MacApp -> Bool
    buildableApp app = any (\e -> exeName e == appName app) buildables
    -- Make a default MacApp in absence of explicit from Setup.hs
    mkDefault x = MacApp (exeName x) Nothing Nothing [] [] DoNotChase

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
appBundleInstallHook apps _ iflags pkg localb = when isMacOS $ do
  let verbosity = fromFlagOrDefault normal (installVerbosity iflags)
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
  , "COUNTER=0"
  , "MAX_DEPTH=256"
  , "ZERO=$0"
  , "NZERO=`readlink $ZERO`; STATUS=$?"
  , ""
  , "# The counter is just a safeguard in case I'd done something silly"
  , "while [ $STATUS -eq 0 -a $COUNTER -lt $MAX_DEPTH ]; do"
  , "  let COUNTER=COUNTER+1"
  , "  ZERO=$NZERO"
  , "  NZERO=`readlink $ZERO`; STATUS=$?"
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
 
isMacOS :: Bool
isMacOS = os == "darwin"

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
     putStrLn $ "Copying executable " ++ appName app ++ " into place"
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
                     let pl = replace "$program" (appName app) plistTemplate
                         pl' = replace "$iconPath" (takeFileName icPath) pl
                     writeFile plDest pl'
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

-- | Perform various magical OS X incantations for turning the app
-- directory into a bundle proper.
osxIncantations ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
osxIncantations appPath app =
  do dtools <- developerTools
     let rez     = dtools </> "Rez"
         setFile = dtools </> "SetFile"
     putStrLn "Running Rez, etc."
     ExitSuccess <- system $ rez ++ " Carbon.r -o " ++
       appPath </> pathInApp app (appName app)
     writeFile (appPath </> "PkgInfo") "APPL????"
     -- Tell Finder about the icon.
     ExitSuccess <- system $ setFile ++ " -a C " ++ appPath </> "Contents"
     return ()

-- | Path to the developer tools directory, if one exists
developerTools :: IO FilePath
developerTools =
   do ds <- filterM doesDirectoryExist cands
      case ds of
        [] -> do putStrLn $ "Can't find the developer tools directory. Have you installed the Developer tools?\n"
                            ++ "I tried looking for:\n" ++ unlines (map ("* " ++) cands)
                 exitWith (ExitFailure 1)
        (d:_) -> return d
  where
    cands = [ "/Applications/XCode.app/Contents/Developer/Tools"
            , "/Developer/Tools"
            ]

-- | Default plist template, based on that in macosx-app from wx (but
-- with version stuff removed).
plistTemplate :: String
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
