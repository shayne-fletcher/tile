module Tile.Geometry
  ( tileRanks
  , tilePoints
  ) where

import Tile.Layout
import Tile.Range
import Tile.Tile

tileRanks :: Tile -> [Int]
tileRanks tile =
  let r = range tile
  in [start r .. end r - 1]

tilePoints :: Layout l => l -> [Int] -> Tile -> [Point]
tilePoints layout fullShape tile =
  [ pointOfRank layout fullShape rank | rank <- tileRanks tile ]
