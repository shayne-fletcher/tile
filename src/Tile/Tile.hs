module Tile.Tile
  ( Tile (..),
    root,
    rootTile,
  )
where

import Tile.Affine
import Tile.Shape

data Tile = Tile
  { space :: AffineRankSpace
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = offset . space

rootTile :: Shape -> Tile
rootTile shp = Tile . rowMajor $ shp
