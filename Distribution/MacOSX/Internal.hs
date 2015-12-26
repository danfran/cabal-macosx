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

module Distribution.MacOSX.Internal (
  osxIncantations
) where

import Control.Exception
import Prelude hiding ( catch )
import System.Cmd ( system )
import System.Exit
import System.FilePath
import Control.Monad (filterM)
import System.Directory (doesDirectoryExist)

import Distribution.MacOSX.Common

-- | Perform various magical OS X incantations for turning the app
-- directory into a bundle proper.
osxIncantations ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
osxIncantations appPath app =
  do dtools <- developerTools

     let rezExec = dtools </> "Rez Carbon.r -o " ++ appPath </> pathInApp app (appName app)
     runCommand rezExec

     writeFile (appPath </> "PkgInfo") "APPL????"

     -- Tell Finder about the icon.
     let setFileExec = dtools </> "SetFile -a C " ++ appPath </> "Contents"
     runCommand setFileExec

     return ()

runCommand :: String -> IO ()
runCommand commandExec =
  do putStrLn $ "Running: " ++ commandExec
     exitValue <- system commandExec
     case exitValue of
        ExitSuccess -> return ()
        _ -> putStrLn $ "Warning - Could not run the command \""
                        ++ commandExec ++ "\". Please check your local `XCode` configuration."

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
