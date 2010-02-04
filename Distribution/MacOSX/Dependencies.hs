{- | Inclusion of bundle-local copies of libraries in application bundles.

This is somewhat experimental.  OS X application bundles can include
local copies of libraries and frameworks (ie dependencies of the
executable) which aids distribution and eases installation.  Xcode and
the traditional OS X development toolchain support this fairly
transparently; this module is an attempt to provide similar
functionality in the cabal-macosx package.

Using this module:

   XXX TODO write this

-}

-- Notes on implementation.
--
-- The basic approach is as follows:
--
--   * Discover the libraries an executable references using
--       otool -L <path>
--
--   * Copy those libraries into the application bundle, at the right
--   place, ie @executable_path/../Frameworks/ where @executable_path
--   represents the path to the exeutable in the bundle.
--
--   * Modify the executable so it refers to the local copy using
--       install_name_tool -change <oldLibPath> <newLibPath> <path>
--   where <newlibPath> points to @executable_path/../Frameworks as
--   described above (@executable_path is a special symbol recognised
--   by the loader).
--
-- Complications:
--
--   * There's some stuff we don't want to include because we can
--   expect it to be present everywhere, eg the Cocoa framework; see
--   "Exclusions", below.
--
--   * Libraries can themselves depend on other libraries; thus, we
--   need to copy them in recursively.
--
--   * Because of these transitive dependencies, dependencies can
--   arise on multiple copies of the same library, in different
--   locations (eg /usr/lib/libfoo and /opt/local/lib/libfoo).  Thus,
--   we preserve that path info, and (for example) copy
--   /usr/lib/libFoo to @executable_path/../Frameworks/usr/lib/
--
-- The approach followed is to build a dependency graph, rooted at the
-- executable, using otool, and then to walk that graph, copying in
-- the libraries, and calling install_name_tool to update the
-- dependencies of entities in the bundle.  Going via a dependency
-- graph is a bit unnecessary - we could just recursively
-- otool/install_name_tool, but its helpful if we need to debug, etc.,
-- and a clear abstraction.
--
-- Exclusions: as described above, a lot of truly common stuff would
-- get copied in, so we provide a mechanism to exclude libraries from
-- this process: @buildDependencyGraph@ can be passed a list of
-- strings, and a library whose path includes any of those strings is
-- excluded.  If no list is passed, the default set is used (it can be
-- inspected/extended if need be).  If an empty list is passed, then
-- nothing is excluded (use with caution).
--
module Distribution.MacOSX.Dependencies (
  Exclusions,
  defaultExclusions,
  includeDependencies,
  DG(..),
  appDependencyGraph
) where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.ParserCombinators.Parsec

import Distribution.MacOSX.Common
import Distribution.MacOSX.DG

includeDependencies :: FilePath -> MacApp -> IO ()
includeDependencies appPath app =
  do dg <- appDependencyGraph appPath app
     let fDeps = dgFDeps dg
     mapM_ (copyInDependency appPath app) fDeps
     mapM_ (updateDependencies appPath app) fDeps

-- | Compute dependency graph for some application.  You won't
-- normally call this directly, except perhaps for testing exclusions,
-- etc.
appDependencyGraph :: FilePath -> MacApp -> IO DG
appDependencyGraph appPath app =
  case (appDeps app) of
    ChaseWithDefaults -> appDependencyGraph appPath app {
                           appDeps = ChaseWith defaultExclusions
                         }
    ChaseWith xs -> do putStrLn "Building dependency graph"
                       buildDependencyGraph' appPath app dgInitial roots [] xs
    DoNotChase -> return dgInitial
  where roots = appName app : otherBins app
        dgInitial = dgEmpty `dgAddPaths` roots

-- | Recursive dependency-graph builder.
buildDependencyGraph' :: FilePath -> MacApp -> DG -> [FilePath] ->
                         [FilePath] -> Exclusions -> IO DG
buildDependencyGraph' _ _ dg [] _ _ = return dg
buildDependencyGraph' appPath app dg (x:xs) done excls =
  do (dg', tgts) <- addFilesDependencies appPath app dg x excls
     let done' = (x:done)
         xs'   = addToQueue xs done' tgts
     buildDependencyGraph' appPath app dg' xs' done' excls
  where addToQueue :: [FilePath] -> [FilePath] -> [FilePath] -> [FilePath]
        addToQueue q done' = foldl (addOneToQueue (q ++ done')) q
        addOneToQueue :: [FilePath] -> [FilePath] -> FilePath -> [FilePath]
        addOneToQueue done' q n = if n `elem` done' then q else q ++ [n]

-- | Given a dependency graph, a path, and a list of exclusions, find
-- the dependencies of the path (minus the exclusions), and add them
-- to the graph, returning that new graph and a list of the dependency
-- targets.
addFilesDependencies :: FilePath -> MacApp -> DG -> FilePath -> Exclusions ->
                        IO (DG, [FilePath])
addFilesDependencies appPath app dg p excls =
  do (FDeps _ tgts) <- getFDeps appPath app p excls
     let dg' = dgAddFDeps dg (FDeps p tgts)
     return (dg', tgts)



oTool, iTool :: FilePath
oTool = "/usr/bin/otool"
iTool = "/usr/bin/install_name_tool"

-- | Get the library dependencies for some file, removing any
-- exclusions.
getFDeps :: FilePath -> MacApp -> FilePath -> Exclusions -> IO FDeps
getFDeps appPath app path exclusions =
  do contents <- readProcess oTool ["-L", absPath] ""
     case parse parseFileDeps "" contents of
       Left err -> error $ show err
       Right fDeps -> return $ exclude exclusions fDeps
  where absPath = if path == appName app then
                    appPath </> pathInApp app (appName app)
                  else path
        parseFileDeps :: Parser FDeps
        parseFileDeps = do f <- manyTill (noneOf ":") (char ':')
                           char '\n'
                           deps <- parseDep `sepEndBy` char '\n'
                           eof
                           return $ FDeps f deps
        parseDep :: Parser FilePath
        parseDep = do char '\t'
                      dep <- manyTill (noneOf " ") (char ' ')
                      char '('
                      manyTill (noneOf ")") (char ')')
                      return dep

-- | Apply an exclusion list to an FDeps; any dependencies which
-- contain any of the exclusions as substrings are excluded.
-- XXX TODO: consider using regexps here?
exclude :: Exclusions -> FDeps -> FDeps
exclude excls (FDeps p ds) = FDeps p $ filter checkExclude ds
  where checkExclude :: FilePath -> Bool
        checkExclude f = not $ any (`isInfixOf` f) excls



copyInDependency :: FilePath -> MacApp -> FDeps -> IO ()
copyInDependency appPath app (FDeps src _) =
  Control.Monad.unless (src == appName app) $
         do putStrLn $ "Copying " ++ src ++ " to " ++ tgt
            createDirectoryIfMissing True $ takeDirectory tgt
            copyFile src tgt
    where tgt = appPath </> pathInApp app src



updateDependencies :: FilePath -> MacApp -> FDeps -> IO ()
updateDependencies appPath app (FDeps src tgts) =
  mapM_ (updateDependency appPath app src) tgts

updateDependency :: FilePath -> MacApp -> FilePath -> FilePath -> IO ()
updateDependency appPath app src tgt =
  do putStrLn $ "Updating " ++ newLib ++ "'s dependency on " ++ tgt ++ " to " ++ tgt'
     let cmd = iTool ++ " -change " ++ show tgt ++ " " ++ show tgt' ++ " " ++ show newLib
     --putStrLn cmd
     runCommand cmd
     return ()
  where tgt' = "@executable_path/../Frameworks/" </> makeRelative "/" tgt
        newLib = if src == appName app then src else appPath </> pathInApp app src
