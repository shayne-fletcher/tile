module Tile.Geometry
  ( tileRanks,
    tilePoints,
  )
where

import Tile.Affine
import Tile.Region
import Tile.Tile

tileRanks :: Tile -> [Int]
tileRanks tile = regionRanks (region tile)

tilePoints :: AffineRankSpace -> Tile -> [Point]
tilePoints rootSpace tile = map (pointOf rootSpace) (tileRanks tile)
