module Tile.Tile
  ( Tile (..),
    root,
    subTile,
    rootTile,
  )
where

import Tile.Range
import Tile.Shape

data Tile = Tile
  { range :: Range,
    shape :: Shape
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = start . range

subTile :: Tile -> Int -> Shape -> Tile
subTile (Tile (Range s _) _) offset dims =
  Tile
    { range = Range (s + offset) (product dims),
      shape = dims
    }

rootTile :: Shape -> Tile
rootTile shp =
  Tile
    { range = Range 0 (size shp),
      shape = shp
    }
