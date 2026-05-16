-- |
-- Module      : Tile.Shape
-- Description : Rank-space dimensions.
--
-- A shape gives the extent of each logical dimension in a rank space.
module Tile.Shape
  ( -- * Shapes
    Shape,
    size,
  )
where

-- | Extents of a logical rank space.
type Shape = [Int]

-- | Total number of points in a shape.
size :: Shape -> Int
size = product
