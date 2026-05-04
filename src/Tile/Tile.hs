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
import Tile.Region

data Tile = Tile
  { region :: Region,
    space :: AffineRankSpace
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = regionOrigin . region

rootTile :: Shape -> Tile
rootTile shp =
  Tile
    { region = contiguous (Range 0 (size shp)),
      space = rowMajor shp
    }

subTile :: Tile -> Int -> Shape -> Tile
subTile parent offset dims =
  Tile
    { region = contiguous (Range (root parent + offset) (product dims)),
      space = rowMajor dims
    }
