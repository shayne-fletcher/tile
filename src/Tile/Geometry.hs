-- |
-- Module      : Tile.Geometry
-- Description : Geometric queries over tiles.
--
-- Geometry queries expose the ranks and logical coordinates covered
-- by a tile.
module Tile.Geometry
  ( -- * Tile geometry
    tileRanks,
    tilePoints,
  )
where

import Tile.Affine
import Tile.Tile

-- | Enumerate the ranks covered by a tile.
tileRanks :: Tile -> [Int]
tileRanks = ranks . space

-- | Enumerate the tile's points in a root coordinate space.
tilePoints :: AffineRankSpace -> Tile -> [Point]
tilePoints rootSpace tile = map (pointOf rootSpace) (tileRanks tile)
