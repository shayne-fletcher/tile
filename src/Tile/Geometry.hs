module Tile.Geometry
  ( tileRanks,
    tilePoints,
  )
where

import Tile.Affine
import Tile.Tile

tileRanks :: Tile -> [Int]
tileRanks = ranks . space

tilePoints :: AffineRankSpace -> Tile -> [Point]
tilePoints rootSpace tile = map (pointOf rootSpace) (tileRanks tile)
