-- | Dependency graph handling.

module Distribution.MacOSX.DG (
  DG(..),
  FDeps(..),
  dgEmpty,
  dgFDeps,
  dgAddPaths,
  dgAddFDeps
) where

import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Tree (Gr)
import Data.List
import Data.Maybe

-- | A dependency graph is an ordinary fgl graph with 'FilePath's on
-- its nodes, and directed edges.  An edge from A to B indicates that
-- A depends on B.
data DG = DG (Gr FilePath ())
          deriving Show

-- | An FDeps represents some file and its list of library
-- dependencies.
data FDeps = FDeps FilePath [FilePath]
             deriving (Eq, Ord, Show)

-- | Add a file dependency to a dependency graph.
dgAddFDeps :: DG -> FDeps -> DG
dgAddFDeps dg (FDeps src tgts) = dgAddDeps src tgts $ dg `dgAddPaths` (src:tgts)

-- | Turn a dependency graph back into a list of FDeps.
dgFDeps :: DG -> [FDeps]
dgFDeps (DG g) = map mkFDep (G.labNodes g)
  where mkFDep :: G.LNode FilePath -> FDeps
        mkFDep (i, src) = FDeps src $ mapMaybe (G.lab g) (G.suc g i)

-- | Create an empty dependency graph.
dgEmpty :: DG
dgEmpty = DG G.empty

-- | Get the node number of a dependency graph node with a specified label.
dgPathIdx :: DG -> FilePath -> Maybe Int
dgPathIdx (DG g) p = case find (\x -> p == snd x) (G.labNodes g) of
                       Just (i, _) -> Just i
                       Nothing -> Nothing

-- | Check if a certain path is already in a dependency graph.
dgHasPath :: DG -> FilePath -> Bool
dgHasPath dg p = case dgPathIdx dg p of
                   Just _ -> True
                   Nothing -> False

-- | Add a list of paths as nodes to a dependency graph, dropping
-- duplicates.
dgAddPaths :: DG -> [FilePath] -> DG
dgAddPaths = foldl dgAddPath

-- | Add a single path as a node to a dependency graph, unless already
-- present.
dgAddPath :: DG -> FilePath -> DG
dgAddPath dg@(DG g) p = if dg `dgHasPath` p then dg
                        else DG $ G.insNode (head $ G.newNodes 1 g, p) g

-- | Given a source path and a list of target paths, add a list of
-- dependencies (ie edges) to a dependency graph.
dgAddDeps :: FilePath -> [FilePath] -> DG -> DG
dgAddDeps src tgts dg = foldl dgAddDep dg $ zip (repeat src) tgts

-- | Add a (src, tgt) dependency (ie edge) to a dependency graph.
dgAddDep :: DG -> (FilePath, FilePath) -> DG
dgAddDep dg@(DG g) (src, tgt) = DG $ G.insEdge (getI src, getI tgt, ()) g
  where getI x = case dgPathIdx dg x of
                   Just x' -> x'
                   Nothing -> error "Can't happen" -- if called in context.
