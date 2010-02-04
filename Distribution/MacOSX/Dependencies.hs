{- | Inclusion of bundle-local copies of libraries in application bundles.

OS X application bundles can include local copies of libraries and
frameworks (ie dependencies of the executable) which aids distribution
and eases installation.  Xcode and the traditional OS X development
toolchain support this fairly transparently; this module is an attempt
to provide similar functionality in the cabal-macosx package.

The basic approach is as follows:

  1. Discover the libraries an object file (executable, other binary, or
     library) references using @otool -L /path/@

  2. Copy those libraries into the application bundle, at the right
     place, ie @\@executable_path\/..\/Frameworks\/@ where
     @\@executable_path@ represents the path to the exeutable in the
     bundle.

  3. Modify the object file so it refers to the local copy, using
     @install_name_tool -change /oldLibPath/ /newLibPath/ /path/@ where
     @/newlibPath/@ points to @\@executable_path\/..\/Frameworks@ as
     described above (@\@executable_path@ is a special symbol recognised
     by the loader).

Complications:

  * There's some stuff we don't want to include because we can
  expect it to be present everywhere, eg the Cocoa framework; see
  /Exclusions/, below.

  * Libraries can themselves depend on other libraries; thus, we
  need to copy them in recursively.

  * Because of these transitive dependencies, dependencies can
  arise on multiple copies of the same library, in different
  locations (eg @\/usr\/lib\/libfoo@ and @\/opt\/local\/lib\/libfoo@).
  Thus, we preserve that path info, and (for example) copy
  @\/usr\/lib\/libFoo@ to
  @\@executable_path\/..\/Frameworks\/usr\/lib\/@.

The approach followed is to build a dependency graph, seeded with the
executable and any other binaries being included in the bundle, using
@otool@; then to walk that graph, copying in the libraries, and
calling @install_name_tool@ to update the dependencies of entities in
the bundle.  Going via a dependency graph is a bit unnecessary - we
could just recursively @otool@/@install_name_tool@, but its helpful if
we need to debug, etc., and a nice clear abstraction.

/Exclusions/: as described above, a lot of truly common stuff would
get copied in, so we provide a mechanism to exclude libraries from
this process: 'buildDependencyGraph' can be passed a list of strings,
and a library whose path includes any of those strings is excluded.
If an empty list is passed, then nothing is excluded (which is almost
certainly not what you want).

-}

module Distribution.MacOSX.Dependencies (
  includeDependencies,
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

-- | Include any library dependencies required in the app.
includeDependencies ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO ()
includeDependencies appPath app =
  do dg <- appDependencyGraph appPath app
     let fDeps = dgFDeps dg
     mapM_ (copyInDependency appPath app) fDeps
     mapM_ (updateDependencies appPath app) fDeps

-- | Compute application's library dependency graph.
appDependencyGraph ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp -> IO DG
appDependencyGraph appPath app =
  case (appDeps app) of
    ChaseWithDefaults -> appDependencyGraph appPath app {
                           appDeps = ChaseWith defaultExclusions
                         }
    ChaseWith xs -> do putStrLn "Building dependency graph"
                       buildDependencyGraph appPath app dgInitial roots [] xs
    DoNotChase -> return dgInitial
  where roots = appName app : otherBins app
        dgInitial = dgEmpty `dgAddPaths` roots

-- | Recursive dependency-graph builder.
buildDependencyGraph ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> DG -- ^ Dependency graph to be extended.
  -> [FilePath] -- ^ Queue of paths to object files to be examined for
                -- dependencies.
  -> [FilePath] -- ^ List of paths of object files which have already
                -- been dealt with.
  -> Exclusions -- ^ List of exclusions for dependency-chasing.
  -> IO DG
buildDependencyGraph _ _ dg [] _ _ = return dg
buildDependencyGraph appPath app dg (x:xs) done excls =
  do (dg', tgts) <- addFilesDependencies appPath app dg x excls
     let done' = (x:done)
         xs'   = addToQueue xs done' tgts
     buildDependencyGraph appPath app dg' xs' done' excls
  where addToQueue :: [FilePath] -> [FilePath] -> [FilePath] -> [FilePath]
        addToQueue q done' = foldl (addOneToQueue (q ++ done')) q
        addOneToQueue :: [FilePath] -> [FilePath] -> FilePath -> [FilePath]
        addOneToQueue done' q n = if n `elem` done' then q else q ++ [n]

-- | Add an object file's dependencies to a dependency graph,
-- returning that new graph and a list of the discovered dependencies.
addFilesDependencies ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> DG -- ^ Dependency graph to be extended.
  -> FilePath -- ^ Path to object file to be examined for dependencies.
  -> Exclusions -- ^ List of exclusions for dependency chasing.
  -> IO (DG, [FilePath])
addFilesDependencies appPath app dg p excls =
  do (FDeps _ tgts) <- getFDeps appPath app p excls
     let dg' = dgAddFDeps dg (FDeps p tgts)
     return (dg', tgts)

-- | Compute the library dependencies for some file, removing any
-- exclusions.
getFDeps ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> FilePath -- ^ Path to object file to be examined for dependencies.
  -> Exclusions -- ^ List of exclusions for dependency chasing.
  -> IO FDeps
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

-- | Apply an exclusion list to an 'FDeps' value; any dependencies
-- which contain any of the exclusions as substrings are excluded.
exclude :: Exclusions -> FDeps -> FDeps
exclude excls (FDeps p ds) = FDeps p $ filter checkExclude ds
  where checkExclude :: FilePath -> Bool
        checkExclude f = not $ any (`isInfixOf` f) excls

-- | Copy some object file's library dependencies into the application
-- bundle.
copyInDependency ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> FDeps -- ^ Dependencies to copy in.
  -> IO ()
copyInDependency appPath app (FDeps src _) =
  Control.Monad.unless (src == appName app) $
         do putStrLn $ "Copying " ++ src ++ " to " ++ tgt
            createDirectoryIfMissing True $ takeDirectory tgt
            copyFile src tgt
    where tgt = appPath </> pathInApp app src

-- | Update some object file's library dependencies to point to
-- bundled copies of libraries.
updateDependencies ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> FDeps -- ^ Dependencies to update.
  -> IO ()
updateDependencies appPath app (FDeps src tgts) =
  mapM_ (updateDependency appPath app src) tgts

-- | Update some object file's dependency on some particular library,
-- to point to the bundled copy of that library.
updateDependency ::
  FilePath -- ^ Path to application bundle root.
  -> MacApp
  -> FilePath -- ^ Path to object file to update.
  -> FilePath -- ^ Path to library which was copied in (path before copy).
  -> IO ()
updateDependency appPath app src tgt =
  do putStrLn $ "Updating " ++ newLib ++ "'s dependency on " ++ tgt ++
                   " to " ++ tgt'
     let cmd = iTool ++ " -change " ++ show tgt ++ " " ++ show tgt' ++
                   " " ++ show newLib
     --putStrLn cmd
     runCommand cmd
     return ()
  where tgt' = "@executable_path/../Frameworks/" </> makeRelative "/" tgt
        newLib = if src == appName app then src
                 else appPath </> pathInApp app src

-- | Path to @otool@ tool.
oTool :: FilePath
oTool = "/usr/bin/otool"

-- | Path to @install_name_tool@ tool.
iTool :: FilePath
iTool = "/usr/bin/install_name_tool"
