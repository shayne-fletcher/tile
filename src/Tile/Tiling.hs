module Tile.Tiling
  ( Tiling (..),
    BlockPartitioning (..),
    Split (..),
    Relation (..),
    TileNode (..),
    childNodes,
    nextHops,
  )
where

import Tile.Affine
import Tile.Tile

class Tiling t where
  children :: t -> Tile -> [Tile]

data Split = Split
  { dim :: Int,
    index :: Int
  }
  deriving (Show, Eq, Ord)

data Relation
  = Root
  | Anchor Split
  | Sibling Split
  deriving (Show, Eq, Ord)

data TileNode = TileNode
  { tile :: Tile,
    relation :: Relation
  }
  deriving (Show, Eq)

fixTileDim :: Tile -> Int -> Int -> Maybe Tile
fixTileDim tile dim i = Tile <$> fixDim (space tile) dim i

childNodes :: BlockPartitioning -> Tile -> [TileNode]
childNodes _ tile = go tile 0
  where
    go t d
      | d >= length (sizes (space t)) = []
      | n <= 1 = go t (d + 1)
      | otherwise =
          let siblings =
                [ TileNode child (Sibling (Split d i))
                | i <- [1 .. n - 1],
                  Just child <- [fixTileDim t d i]
                ]
              anchors =
                [ TileNode child (Anchor (Split d 0))
                | Just child <- [fixTileDim t d 0]
                ]
           in siblings ++ anchors
      where
        n = sizes (space t) !! d

rootNode :: Tile -> TileNode
rootNode t = TileNode t Root

nextHops :: BlockPartitioning -> TileNode -> [TileNode]
nextHops tiling node = project (childNodes tiling (tile node))
  where
    project [] = []
    project (child : rest) =
      case relation child of
        Root -> project rest
        Sibling _ -> child : project rest
        Anchor _ -> nextHops tiling child ++ project rest

data BlockPartitioning = BlockPartitioning
  deriving (Show, Eq)

instance Tiling BlockPartitioning where
  children tiling = map tile . nextHops tiling . rootNode
