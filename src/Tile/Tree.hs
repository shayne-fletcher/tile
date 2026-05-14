-- |
-- Module      : Tile.Tree
-- Description : Communication structure materialized as explicit
-- trees.
--
-- The pipeline converts a 'Tiling' into a 'Schedule' through a
-- sequence of tree transformations:
--
-- @
-- 'decompositionTree' → 'contractAnchors' → 'hopTree' → 'sendTree'
-- @
--
-- The generic spine ('Tree', 'unfoldTree', 'mapTree') is the common
-- substrate for all four views.
module Tile.Tree
  ( -- * Generic tree
    Tree (..),
    mapTree,
    unfoldTree,
    treeLabels,
    treeIndex,
    renderTreeWith,

    -- * Tile tree views
    DecompositionView,
    HopView,
    TileTree (..),
    DecompositionTree,
    HopTree,
    SendTree (..),
    RoutedTree (..),

    -- * Tile pipeline
    decompositionTree,
    contractAnchors,
    hopTree,
    sendTree,
    children,

    -- * Schedule trees
    scheduleTree,
    routedTree,

    -- * Rendering
    renderDecompositionTree,
    renderHopTree,
    renderSendTree,
    renderRoutedTree,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Tile.Geometry
import Tile.Schedule
import Tile.Tile
import Tile.Tiling

-- | A rose tree: a label with an ordered list of subtrees.
data Tree a = Tree
  { treeLabel :: a,
    subtrees :: [Tree a]
  }
  deriving (Show, Eq)

-- | Phantom type distinguishing a structural decomposition tree.
data DecompositionView

-- | Phantom type distinguishing a hop tree.
data HopView

-- | A tree of 'TileNode's tagged with a phantom view type to prevent
-- mixing structurally distinct tree views.
newtype TileTree view = TileTree
  { getTileTree :: Tree TileNode
  }
  deriving (Show, Eq)

-- | A structural decomposition tree: every node carries the full
-- 'Relation' metadata from its parent split.
type DecompositionTree = TileTree DecompositionView

-- | A hop tree: anchor nodes have been contracted; every edge is a
-- communication hop.
type HopTree = TileTree HopView

-- | A tree of 'Tile's whose roots are the communication destinations.
-- Each parent–child edge corresponds to one 'Step' in the fault-free
-- schedule.
newtype SendTree = SendTree
  { getSendTree :: Tree Tile
  }
  deriving (Show, Eq)

-- | A tree of schedule members reconstructed from a 'RoutedSchedule'.
newtype RoutedTree a = RoutedTree
  { getRoutedTree :: Tree a
  }
  deriving (Show, Eq)

-- | Unfold the structural decomposition of a tile using 'childNodes'.
-- Every node in the result carries its 'Relation' to its parent.
decompositionTree :: (Tiling t) => t -> Tile -> DecompositionTree
decompositionTree tiling baseTile =
  TileTree $
    unfoldTree
      (childNodes tiling . tile)
      (TileNode baseTile Root)

-- | Convert a 'DecompositionTree' into a 'HopTree' by contracting
-- anchor edges. Anchor nodes are spliced out and their sibling
-- descendants promoted; the result contains only communication hops.
contractAnchors :: DecompositionTree -> HopTree
contractAnchors (TileTree tree) = TileTree (go tree)
  where
    go (Tree node kids) = Tree node (concatMap project kids)
    project subtree@(Tree node kids) =
      case relation node of
        Sibling _ -> [go subtree]
        Anchor _ -> concatMap project kids
        Root -> []

-- | Build the hop tree for a tile: structural decomposition followed
-- by anchor contraction.
--
-- @
-- hopTree = contractAnchors . decompositionTree
-- @
hopTree :: (Tiling t) => t -> Tile -> HopTree
hopTree tiling = contractAnchors . decompositionTree tiling

-- | Project a 'HopTree' to a tree of communication-root 'Tile's.
sendTree :: (Tiling t) => t -> Tile -> SendTree
sendTree tiling =
  SendTree . mapTree tile . getTileTree . hopTree tiling

-- | Direct communication children of a tile: the roots of the
-- subtiles in its 'SendTree'.
children :: (Tiling t) => t -> Tile -> [Tile]
children tiling =
  map treeLabel . subtrees . getSendTree . sendTree tiling

-- | Reconstruct a broadcast tree from a schedule by following
-- sender-to-receiver edges in 'adjacencyList' order.
scheduleTree :: (Ord a) => a -> Schedule a -> RoutedTree a
scheduleTree ingress schedule =
  RoutedTree (unfoldTree childrenOf ingress)
  where
    childrenOf member =
      Map.findWithDefault [] member (adjacencyList schedule)

-- | Build a 'RoutedTree' from a 'RoutedSchedule'.
routedTree :: (Ord a) => RoutedSchedule a -> RoutedTree a
routedTree RoutedSchedule {ingress = member, routedSteps = steps} =
  scheduleTree member steps

-- | Render a 'DecompositionTree' with numbered tile nodes.
renderDecompositionTree :: [String] -> DecompositionTree -> String
renderDecompositionTree members (TileTree tree) =
  unlines (renderTree [] tree)
  where
    tileIds = zip (sortOn pathKey (decompositionPaths [] tree)) [0 ..]

    renderTree path (Tree node kids) =
      renderNumberedNode members (tileId tileIds path) node
        : renderChildren path kids

    renderChildren _ [] = []
    renderChildren path kids =
      concat
        [ renderBranch prefix child
        | (prefix, child) <- branchPrefixes kids
        ]
      where
        renderBranch prefix child =
          case renderTree (path ++ relationPath (treeLabel child)) child of
            [] -> []
            first : rest ->
              (prefix ++ first)
                : [continuation prefix ++ line | line <- rest]

    pathKey path = (length path, path)

-- | Render a 'HopTree' showing each tile's member set.
renderHopTree :: [String] -> HopTree -> String
renderHopTree members (TileTree tree) =
  renderTreeWith (renderTile members . tile) tree

-- | Render a 'SendTree' showing each tile's communication root.
renderSendTree :: [String] -> SendTree -> String
renderSendTree members (SendTree tree) =
  renderTreeWith (renderMember members) tree

-- | Render a 'RoutedTree' of strings.
renderRoutedTree :: RoutedTree String -> String
renderRoutedTree (RoutedTree tree) =
  renderTreeWith id tree

-- | Apply a function to every label in a tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Tree label kids) =
  Tree
    { treeLabel = f label,
      subtrees = map (mapTree f) kids
    }

-- | Build a tree from a seed by repeatedly applying a child function.
unfoldTree :: (a -> [a]) -> a -> Tree a
unfoldTree childrenOf label =
  Tree
    { treeLabel = label,
      subtrees = map (unfoldTree childrenOf) (childrenOf label)
    }

-- | Collect every label in a tree.
treeLabels :: (Ord a) => Tree a -> Set.Set a
treeLabels (Tree label kids) =
  Set.insert label (Set.unions (map treeLabels kids))

-- | Index every subtree by its root label.
treeIndex :: (Ord a) => Tree a -> Map.Map a (Tree a)
treeIndex tree@(Tree label kids) =
  Map.insert label tree (Map.unions (map treeIndex kids))

-- | Render a tree as an ASCII box-drawing string.
renderTreeWith :: (a -> String) -> Tree a -> String
renderTreeWith renderLabel tree =
  unlines (renderLines tree)
  where
    renderLines (Tree label kids) =
      renderLabel label : renderChildren kids

    renderChildren [] = []
    renderChildren kids =
      concat
        [ renderBranch prefix child
        | (prefix, child) <- branchPrefixes kids
        ]

    renderBranch prefix child =
      case renderLines child of
        [] -> []
        first : rest ->
          (prefix ++ first)
            : [continuation prefix ++ line | line <- rest]

branchPrefixes :: [a] -> [(String, a)]
branchPrefixes [] = []
branchPrefixes [x] = [("└─ ", x)]
branchPrefixes (x : xs) = ("├─ ", x) : branchPrefixes xs

continuation :: String -> String
continuation "├─ " = "│  "
continuation "└─ " = "   "
continuation prefix = replicate (length prefix) ' '

decompositionPaths :: [Split] -> Tree TileNode -> [[Split]]
decompositionPaths path (Tree _ kids) =
  path : concat [decompositionPaths (path ++ relationPath (treeLabel child)) child | child <- kids]

relationPath :: TileNode -> [Split]
relationPath node =
  case relation node of
    Root -> []
    Anchor split -> [split]
    Sibling split -> [split]

tileId :: [([Split], Int)] -> [Split] -> Int
tileId ids path =
  case lookup path ids of
    Just ident -> ident
    Nothing -> error "tileId: missing path"

renderNumberedNode :: [String] -> Int -> TileNode -> String
renderNumberedNode members ident node =
  "T" ++ show ident ++ " " ++ renderTile members (tile node)

renderTile :: [String] -> Tile -> String
renderTile members tile =
  "[" ++ unwords [members !! rank | rank <- tileRanks tile] ++ "]"

renderMember :: [String] -> Tile -> String
renderMember members tile =
  members !! root tile
