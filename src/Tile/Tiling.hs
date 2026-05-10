module Tile.Tiling
  ( Tiling (..),
    BlockPartitioning (..),
    Bisection (..),
    Split (..),
    Relation (..),
    TileNode (..),
    nextHops,
  )
where

import Tile.Affine
import Tile.Tile

class Tiling t where
  childNodes :: t -> Tile -> [TileNode]

  children :: t -> Tile -> [Tile]
  children tiling = map tile . nextHops tiling . rootNode

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

selectTileDim :: Tile -> Int -> Int -> Int -> Maybe Tile
selectTileDim tile dim begin end =
  Tile <$> select (space tile) dim begin end 1

rootNode :: Tile -> TileNode
rootNode t = TileNode t Root

nextHops :: (Tiling t) => t -> TileNode -> [TileNode]
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

data Bisection = Bisection
  deriving (Show, Eq)

instance Tiling Bisection where
  childNodes _ tile =
    case firstNonSingletonDim tile of
      Nothing -> []
      Just d ->
        let n = sizes (space tile) !! d
            lower = n `div` 2
            siblings =
              [ TileNode child (Sibling (Split d lower))
              | Just child <- [selectTileDim tile d lower n]
              ]
            anchors =
              [ TileNode child (Anchor (Split d 0))
              | Just child <- [selectTileDim tile d 0 lower]
              ]
         in siblings ++ anchors

firstNonSingletonDim :: Tile -> Maybe Int
firstNonSingletonDim tile =
  go 0 (sizes (space tile))
  where
    go _ [] = Nothing
    go d (n : ns)
      | n > 1 = Just d
      | otherwise = go (d + 1) ns
