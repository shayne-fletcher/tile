module Tile.Routing
  ( Traversal (..),
    buildScheduleFrom,
    buildSchedule,
    buildOccludedScheduleFrom,
  )
where

import Tile.Schedule
import Tile.Shape
import Tile.Tile
import Tile.Tiling
import Tile.Tree

data Traversal = DFS | BFS
  deriving (Show, Eq)

buildScheduleFrom :: (Tiling t) => Traversal -> t -> [a] -> Tile -> Schedule a
buildScheduleFrom traversal tiling members startTile =
  let SendTree tree = sendTree tiling startTile
   in case traversal of
        DFS -> dfsFaultFree members tree
        BFS -> bfsFaultFree members [tree]

buildSchedule :: (Tiling t) => Traversal -> t -> [a] -> Shape -> Schedule a
buildSchedule traversal tiling members shp =
  buildScheduleFrom traversal tiling members (rootTile shp)

buildOccludedScheduleFrom ::
  (Tiling t, Eq a) =>
  Traversal ->
  Occlusion a ->
  t ->
  [a] ->
  Tile ->
  Maybe (RoutedSchedule a)
buildOccludedScheduleFrom traversal occ tiling members startTile = do
  let SendTree tree = sendTree tiling startTile
      prunedTree = pruneOccluded occ members tree
  startRep <- representative occ members (treeLabel prunedTree)
  let steps = case traversal of
        DFS -> dfsOccluded occ members prunedTree
        BFS -> bfsOccluded occ members [prunedTree]
  pure RoutedSchedule {ingress = startRep, routedSteps = steps}

pruneOccluded :: (Eq a) => Occlusion a -> [a] -> Tree Tile -> Tree Tile
pruneOccluded occ members (Tree t kids) =
  Tree t (concatMap project kids)
  where
    project subtree@(Tree childTile _)
      | Just _ <- representative occ members childTile = [pruneOccluded occ members subtree]
      | otherwise = []

dfsFaultFree :: [a] -> Tree Tile -> Schedule a
dfsFaultFree members (Tree t kids) =
  let steps = [step | child <- kids, Just step <- [stepFor members t (treeLabel child)]]
   in steps ++ concatMap (dfsFaultFree members) kids

bfsFaultFree :: [a] -> [Tree Tile] -> Schedule a
bfsFaultFree _ [] = []
bfsFaultFree members trees =
  let steps = [step | Tree t kids <- trees, child <- kids, Just step <- [stepFor members t (treeLabel child)]]
      nextLevel = concatMap subtrees trees
   in steps ++ bfsFaultFree members nextLevel

dfsOccluded :: (Eq a) => Occlusion a -> [a] -> Tree Tile -> Schedule a
dfsOccluded occ members (Tree t kids) =
  let steps = [step | child <- kids, Just step <- [stepForOccluded occ members t (treeLabel child)]]
   in steps ++ concatMap (dfsOccluded occ members) kids

bfsOccluded :: (Eq a) => Occlusion a -> [a] -> [Tree Tile] -> Schedule a
bfsOccluded _ _ [] = []
bfsOccluded occ members trees =
  let steps = [step | Tree t kids <- trees, child <- kids, Just step <- [stepForOccluded occ members t (treeLabel child)]]
      nextLevel = concatMap subtrees trees
   in steps ++ bfsOccluded occ members nextLevel
