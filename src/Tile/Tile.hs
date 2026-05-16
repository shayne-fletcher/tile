-- |
-- Module      : Tile.Tile
-- Description : Affine tiles.
--
-- A tile is an affine view of ranks. It carries geometry only: its
-- root is the rank at the affine origin.
module Tile.Tile
  ( -- * Tiles
    Tile (..),
    root,
    rootTile,
  )
where

import Tile.Affine
import Tile.Shape

-- | A tile represented by an affine rank space.
newtype Tile = Tile
  { -- | Affine rank space covered by the tile.
    space :: AffineRankSpace
  }
  deriving (Show, Eq)

-- | Root rank of a tile.
--
-- This is the offset of the tile's affine rank space.
root :: Tile -> Int
root = offset . space

-- | Construct the root tile for a row-major shape.
rootTile :: Shape -> Tile
rootTile = Tile . rowMajor
