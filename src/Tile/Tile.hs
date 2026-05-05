module Tile.Tile
  ( Tile (..),
    root,
    rootTile,
  )
where

import Tile.Range
import Tile.Shape
import Tile.Affine

data Tile = Tile
  { space :: AffineRankSpace
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = offset . space

rootTile :: Shape -> Tile
rootTile shp = Tile . rowMajor $ shp
