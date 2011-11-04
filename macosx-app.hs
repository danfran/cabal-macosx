module Main where

import Distribution.MacOSX
import Distribution.MacOSX.AppBuildInfo
import System.Directory
import System.Environment
import System.FilePath

main = do
  pname <- getProgName
  xs    <- getArgs
  exe <- case xs of
           [x1] -> return x1 
           _    -> fail $ "Usage: " ++ pname ++ " <exe>"
  exeExists <- doesFileExist exe
  let macapp  = MacApp { appName   = takeFileName exe
                       , appIcon   = Nothing
                       , appPlist  = Nothing
                       , resources = []
                       , otherBins = []
                       , appDeps   = DoNotChase
                       } 
      appInfo = AppBuildInfo { abApp        = macapp
                             , abAppPath    = appName macapp <.> "app"
                             , abAppOrigExe = exe
                             }
  if exeExists
     then makeAppBundle appInfo
     else fail $ exe ++ " does not exist"
