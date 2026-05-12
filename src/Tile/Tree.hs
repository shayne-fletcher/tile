module Tile.Tree
  ( Tree (..),
    DecompositionView,
    HopView,
    TileTree (..),
    DecompositionTree,
    HopTree,
    mapTree,
    unfoldTree,
    renderTreeWith,
    SendTree (..),
    RoutedTree (..),
    scheduleTree,
    routedTree,
    renderRoutedTree,
    decompositionTree,
    contractAnchors,
    hopTree,
    sendTree,
    children,
    renderDecompositionTree,
    renderHopTree,
    renderSendTree,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Tile.Geometry
import Tile.Schedule
import Tile.Tile
import Tile.Tiling

data Tree a = Tree
  { treeLabel :: a,
    subtrees :: [Tree a]
  }
  deriving (Show, Eq)

data DecompositionView

data HopView

newtype TileTree view = TileTree
  { getTileTree :: Tree TileNode
  }
  deriving (Show, Eq)

type DecompositionTree = TileTree DecompositionView

type HopTree = TileTree HopView

newtype SendTree = SendTree
  { getSendTree :: Tree Tile
  }
  deriving (Show, Eq)

newtype RoutedTree a = RoutedTree
  { getRoutedTree :: Tree a
  }
  deriving (Show, Eq)

decompositionTree :: (Tiling t) => t -> Tile -> DecompositionTree
decompositionTree tiling baseTile =
  TileTree $
    unfoldTree
      (childNodes tiling . tile)
      (TileNode baseTile Root)

contractAnchors :: DecompositionTree -> HopTree
contractAnchors (TileTree tree) = TileTree (go tree)
  where
    go (Tree node kids) = Tree node (concatMap project kids)
    project subtree@(Tree node kids) =
      case relation node of
        Sibling _ -> [go subtree]
        Anchor _ -> concatMap project kids
        Root -> []

hopTree :: (Tiling t) => t -> Tile -> HopTree
hopTree tiling = contractAnchors . decompositionTree tiling

sendTree :: (Tiling t) => t -> Tile -> SendTree
sendTree tiling =
  SendTree . mapTree tile . getTileTree . hopTree tiling

children :: (Tiling t) => t -> Tile -> [Tile]
children tiling =
  map treeLabel . subtrees . getSendTree . sendTree tiling

scheduleTree :: (Ord a) => a -> Schedule a -> RoutedTree a
scheduleTree ingress schedule =
  RoutedTree (unfoldTree childrenOf ingress)
  where
    childrenOf member =
      Map.findWithDefault [] member (adjacencyList schedule)

routedTree :: (Ord a) => RoutedSchedule a -> RoutedTree a
routedTree RoutedSchedule {ingress = member, routedSteps = steps} =
  scheduleTree member steps

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

renderHopTree :: [String] -> HopTree -> String
renderHopTree members (TileTree tree) =
  renderTreeWith (renderTile members . tile) tree

renderSendTree :: [String] -> SendTree -> String
renderSendTree members (SendTree tree) =
  renderTreeWith (renderMember members) tree

renderRoutedTree :: RoutedTree String -> String
renderRoutedTree (RoutedTree tree) =
  renderTreeWith id tree

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Tree label kids) =
  Tree
    { treeLabel = f label,
      subtrees = map (mapTree f) kids
    }

unfoldTree :: (a -> [a]) -> a -> Tree a
unfoldTree childrenOf label =
  Tree
    { treeLabel = label,
      subtrees = map (unfoldTree childrenOf) (childrenOf label)
    }

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
