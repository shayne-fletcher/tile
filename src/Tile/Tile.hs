module Tile.Tile
  ( Tile (..),
    root,
    rootTile,
  )
where

import Tile.Affine
import Tile.Shape

newtype Tile = Tile
  { space :: AffineRankSpace
  }
  deriving (Show, Eq)

root :: Tile -> Int
root = offset . space

rootTile :: Shape -> Tile
rootTile = Tile . rowMajor
