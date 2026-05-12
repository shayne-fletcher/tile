-- |
-- Module      : Tile.Tiling
-- Description : Recursive decompositions of affine tiles.
--
-- A tiling defines the structural decomposition of a tile. The tree
-- layer derives communication children by contracting anchor edges.
module Tile.Tiling
  ( -- * Tiling
    Tiling (..),

    -- * Built-in tilings
    BlockPartitioning (..),
    Bisection (..),

    -- * Decomposition nodes
    Split (..),
    Relation (..),
    TileNode (..),
  )
where

import Tile.Affine
import Tile.Tile

-- | A recursive decomposition of an affine 'Tile'.
--
-- A 'Tiling' defines the structural children of a tile. Each
-- structural child is labelled by its relationship to the parent: an
-- 'Anchor' child contains the parent root, while a 'Sibling' child
-- introduces a distinct communication root.
--
-- A lawful 'Tiling' satisfies:
--
-- [Inclusion]
--   Every structural child is contained in its parent.
--
--   @
--   tileRanks child \`isSubsetOf\` tileRanks parent
--   @
--
-- [Structural cover]
--   For a non-terminal parent, the structural children partition the
--   parent ranks: their ranks are pairwise disjoint and their union
--   is the parent.
--
-- [Progress]
--   Every structural child is strictly smaller than its parent.
--
--   @
--   length (tileRanks child) < length (tileRanks parent)
--   @
--
-- [Anchor preservation]
--   An anchor child preserves the parent root.
--
--   @
--   case relation node of
--     Anchor _ -> root (tile node) == root parent
--     _        -> True
--   @
--
-- [Sibling movement]
--   A sibling child has a distinct root from its parent.
--
--   @
--   case relation node of
--     Sibling _ -> root (tile node) /= root parent
--     _         -> True
--   @
class Tiling t where
  -- | Structural children of a tile, labelled by their decomposition
  -- relation. Anchor children carry the parent root; sibling children
  -- introduce a distinct communication root.
  childNodes :: t -> Tile -> [TileNode]

-- | The affine dimension and starting index selected by a tiling
-- step.
--
-- A 'Split' records where a child tile came from inside its parent.
data Split = Split
  { -- | Dimension split by the tiling step.
    dim :: Int,
    -- | Starting index of the child along 'dim'.
    index :: Int
  }
  deriving (Show, Eq, Ord)

-- | Relationship between a 'TileNode' and its parent.
--
-- Relations distinguish geometry-only anchor steps from communication
-- steps. The tree layer contracts anchor edges; sibling edges become
-- communication edges.
data Relation
  = -- | The root of a decomposition tree.
    Root
  | -- | A child that preserves the parent root.
    Anchor Split
  | -- | A child with a distinct root from the parent.
    Sibling Split
  deriving (Show, Eq, Ord)

-- | A tile labelled with its relationship to a parent tile.
--
-- 'TileNode' is the node type used by decomposition and hop trees.
-- The 'relation' field records how the node arises from its parent;
-- the root node of a tree uses 'Root'.
data TileNode = TileNode
  { -- | Tile carried by the node.
    tile :: Tile,
    -- | Relationship to the parent node.
    relation :: Relation
  }
  deriving (Show, Eq)

-- | Fix one affine dimension of a tile to a single index.
fixTileDim :: Tile -> Int -> Int -> Maybe Tile
fixTileDim tile dim i = Tile <$> fixDim (space tile) dim i

-- | Select a contiguous interval along one tile dimension.
selectTileDim :: Tile -> Int -> Int -> Int -> Maybe Tile
selectTileDim tile dim begin end =
  Tile <$> select (space tile) dim begin end 1

-- | Partition by fixing one coordinate at a time.
--
-- 'BlockPartitioning' finds the first non-singleton dimension. It
-- creates one anchor child at index @0@ and one sibling child for
-- each remaining index in that dimension.
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

-- | Partition by bisecting one dimension at a time.
--
-- 'Bisection' finds the first non-singleton dimension. It keeps the
-- lower @floor(n / 2)@ half as the anchor and creates one sibling
-- from the remaining upper half.
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
