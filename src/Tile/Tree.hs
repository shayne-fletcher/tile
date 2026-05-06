module Tile.Tree
  ( TileNodeTree (..),
    decompositionTree,
    renderDecompositionTree,
  )
where

import Data.List (sortOn)
import Tile.Geometry
import Tile.Tile
import Tile.Tiling

data TileNodeTree = TileNodeTree
  { node :: TileNode,
    subtrees :: [TileNodeTree]
  }
  deriving (Show, Eq)

decompositionTree :: BlockPartitioning -> Tile -> TileNodeTree
decompositionTree tiling baseTile =
  go (TileNode baseTile Root)
  where
    go node =
      TileNodeTree
        { node = node,
          subtrees =
            [ go child
            | child <- childNodes tiling (tile node)
            ]
        }

renderDecompositionTree :: [String] -> TileNodeTree -> String
renderDecompositionTree members tree =
  unlines (renderTree [] tree)
  where
    tileIds = zip (sortOn pathKey (paths [] tree)) [0 ..]

    renderTree path (TileNodeTree node kids) =
      renderNode members (tileId tileIds path) node
        : renderChildren path kids

    renderChildren _ [] = []
    renderChildren path kids =
      concat
        [ renderBranch prefix child
        | (prefix, child) <- branchPrefixes kids
        ]
      where
        renderBranch prefix child =
          case renderTree (path ++ relationPath (node child)) child of
            [] -> []
            first : rest ->
              (prefix ++ first)
                : [continuation prefix ++ line | line <- rest]

    pathKey path = (length path, path)

branchPrefixes :: [a] -> [(String, a)]
branchPrefixes [] = []
branchPrefixes [x] = [("└─ ", x)]
branchPrefixes (x : xs) = ("├─ ", x) : branchPrefixes xs

continuation :: String -> String
continuation "├─ " = "│  "
continuation "└─ " = "   "
continuation prefix = replicate (length prefix) ' '

paths :: [Split] -> TileNodeTree -> [[Split]]
paths path (TileNodeTree _ kids) =
  path : concat [paths (path ++ relationPath (node child)) child | child <- kids]

relationPath :: TileNode -> [Split]
relationPath tileNode =
  case relation tileNode of
    Root -> []
    Anchor split -> [split]
    Sibling split -> [split]

tileId :: [([Split], Int)] -> [Split] -> Int
tileId ids path =
  case lookup path ids of
    Just ident -> ident
    Nothing -> error "tileId: missing path"

renderNode :: [String] -> Int -> TileNode -> String
renderNode members ident tileNode =
  "T" ++ show ident ++ " " ++ renderTile members (tile tileNode)

renderTile :: [String] -> Tile -> String
renderTile members tile =
  "[" ++ unwords [members !! rank | rank <- tileRanks tile] ++ "]"
