module Tile.Tile
  ( Tile (..),
    root,
    subTile,
    rootTile,
  )
where

import Tile.Range
import Tile.Shape
import Tile.Affine

data Tile = Tile
  { range :: Range,
    space :: AffineRankSpace
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = start . range

rootTile :: Shape -> Tile
rootTile shp =
  Tile
    { range = Range 0 (size shp),
      space = rowMajor shp
    }

subTile :: Tile -> Int -> Shape -> Tile
subTile (Tile (Range s _) _) offset dims =
  Tile
    { range = Range (s + offset) (product dims),
      space = rowMajor dims
    }
