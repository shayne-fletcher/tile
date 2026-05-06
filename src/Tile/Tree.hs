module Tile.Tree
  ( DecompositionTree (..),
    HopTree (..),
    SendTree (..),
    decompositionTree,
    hopTree,
    sendTree,
    renderDecompositionTree,
    renderHopTree,
    renderSendTree,
  )
where

import Data.List (sortOn)
import Tile.Geometry
import Tile.Tile
import Tile.Tiling

data DecompositionTree = DecompositionTree
  { decompositionNode :: TileNode,
    decompositionSubtrees :: [DecompositionTree]
  }
  deriving (Show, Eq)

data HopTree = HopTree
  { hopNode :: TileNode,
    hopSubtrees :: [HopTree]
  }
  deriving (Show, Eq)

data SendTree = SendTree
  { sendTile :: Tile,
    sendSubtrees :: [SendTree]
  }
  deriving (Show, Eq)

decompositionTree :: BlockPartitioning -> Tile -> DecompositionTree
decompositionTree tiling baseTile =
  go (TileNode baseTile Root)
  where
    go node =
      DecompositionTree
        { decompositionNode = node,
          decompositionSubtrees =
            [ go child
            | child <- childNodes tiling (tile node)
            ]
        }

hopTree :: BlockPartitioning -> Tile -> HopTree
hopTree tiling baseTile =
  go (TileNode baseTile Root)
  where
    go node =
      HopTree
        { hopNode = node,
          hopSubtrees =
            [ go child
            | child <- nextHops tiling node
            ]
        }

sendTree :: BlockPartitioning -> Tile -> SendTree
sendTree tiling baseTile =
  fromHopTree (hopTree tiling baseTile)

fromHopTree :: HopTree -> SendTree
fromHopTree (HopTree node kids) =
  SendTree
    { sendTile = tile node,
      sendSubtrees = map fromHopTree kids
    }

renderDecompositionTree :: [String] -> DecompositionTree -> String
renderDecompositionTree members tree =
  unlines (renderTree [] tree)
  where
    tileIds = zip (sortOn pathKey (decompositionPaths [] tree)) [0 ..]

    renderTree path (DecompositionTree node kids) =
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
          case renderTree (path ++ relationPath (decompositionNode child)) child of
            [] -> []
            first : rest ->
              (prefix ++ first)
                : [continuation prefix ++ line | line <- rest]

    pathKey path = (length path, path)

renderHopTree :: [String] -> HopTree -> String
renderHopTree members tree =
  unlines (renderTree tree)
  where
    renderTree (HopTree node kids) =
      renderTile members (tile node)
        : renderChildren kids

    renderChildren [] = []
    renderChildren kids =
      concat
        [ renderBranch prefix child
        | (prefix, child) <- branchPrefixes kids
        ]

    renderBranch prefix child =
      case renderTree child of
        [] -> []
        first : rest ->
          (prefix ++ first)
            : [continuation prefix ++ line | line <- rest]

renderSendTree :: [String] -> SendTree -> String
renderSendTree members tree =
  unlines (renderTree tree)
  where
    renderTree (SendTree tile kids) =
      renderMember members tile
        : renderChildren kids

    renderChildren [] = []
    renderChildren kids =
      concat
        [ renderBranch prefix child
        | (prefix, child) <- branchPrefixes kids
        ]

    renderBranch prefix child =
      case renderTree child of
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

decompositionPaths :: [Split] -> DecompositionTree -> [[Split]]
decompositionPaths path (DecompositionTree _ kids) =
  path : concat [decompositionPaths (path ++ relationPath (decompositionNode child)) child | child <- kids]

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
