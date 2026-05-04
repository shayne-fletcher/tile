module Tile.Geometry
  ( tileRanks,
    tilePoints,
  )
where

import Tile.Layout
import Tile.Region
import Tile.Tile

tileRanks :: Tile -> [Int]
tileRanks tile = regionRanks (region tile)

tilePoints :: (Layout l) => l -> [Int] -> Tile -> [Point]
tilePoints layout fullShape tile =
  [pointOfRank layout fullShape rank | rank <- tileRanks tile]
