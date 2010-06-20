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
  MacApp(..),
  ChaseDeps(..),
  Exclusions,
  defaultExclusions
) where

import Control.Monad (forM_)
import Data.String.Utils (replace)
import Distribution.PackageDescription (PackageDescription(..),
                                        Executable(..))
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags)
import System.Cmd (system)
import System.FilePath
import System.Info (os)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit

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
  case os of
    "darwin" -> forM_ apps' $ makeAppBundle localb
      where apps' = case apps of
                      [] -> map mkDefault $ executables pkg
                      xs -> xs
            mkDefault x = MacApp (exeName x) Nothing Nothing [] [] DoNotChase
    _ -> putStrLn "Not OS X, so not building an application bundle."

-- | Given a 'MacApp' in context, make an application bundle in the
-- build area.
makeAppBundle ::
  LocalBuildInfo -> MacApp -> IO ()
makeAppBundle localb app =
  do appPath <- createAppDir localb app
     maybeCopyPlist appPath app
     maybeCopyIcon appPath app
       `catch` \err -> putStrLn ("Warning: could not set up icon for " ++
                                 appName app ++ ": " ++ show err)
     includeResources appPath app
     includeDependencies appPath app
     osxIncantations appPath app

-- | Create application bundle directory structure in build directory
-- and copy executable into it.  Returns path to newly created
-- directory.
createAppDir :: LocalBuildInfo -> MacApp -> IO FilePath
createAppDir localb app =
  do putStrLn $ "Creating application bundle directory " ++ appPath
     createDirectoryIfMissing False appPath
     createDirectoryIfMissing True  $ takeDirectory exeDest
     createDirectoryIfMissing True  $ appPath </> "Contents/Resources"
     putStrLn $ "Copying executable " ++ appName app ++ " into place"
     copyFile exeSrc exeDest
     return appPath
  where appPath = buildDir localb </> appName app <.> "app"
        exeDest = appPath </> pathInApp app (appName app)
        exeSrc = buildDir localb </> appName app </> appName app

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
  do putStrLn "Running Rez, etc."
     ExitSuccess <- system $ rez ++ " Carbon.r -o " ++
       appPath </> pathInApp app (appName app)
     writeFile (appPath </> "PkgInfo") "APPL????"
     -- Tell Finder about the icon.
     ExitSuccess <- system $ setFile ++ " -a C " ++ appPath </> "Contents"
     return ()

-- | Path to @Rez@ tool.
rez :: FilePath
rez = "/Developer/Tools/Rez"

-- | Path to @SetFile@ tool.
setFile :: FilePath
setFile = "/Developer/Tools/SetFile"

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
