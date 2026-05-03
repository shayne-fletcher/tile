module Tile.Shape
  ( Shape,
    size,
  )
where

type Shape = [Int]

size :: Shape -> Int
size = product
