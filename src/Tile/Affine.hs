module Tile.Affine
  ( AffineRankSpace (..),
    rowMajor,
    rankOf,
    pointOf,
    spaceExtent,
  )
where

import Tile.Layout (Point)
import Tile.Shape (Shape)

data AffineRankSpace = AffineRankSpace
  { offset :: Int,
    sizes :: [Int],
    strides :: [Int]
  }
  deriving (Show, Eq)

rowMajor :: Shape -> AffineRankSpace
rowMajor shape =
  AffineRankSpace
    { offset = 0,
      sizes = shape,
      strides = tail (scanr (*) 1 shape)
    }

rankOf :: AffineRankSpace -> Point -> Int
rankOf space coord =
  offset space + sum (zipWith (*) coord (strides space))

pointOf :: AffineRankSpace -> Int -> Point
pointOf space rank =
  zipWith coordinate (strides space) (sizes space)
  where
    relativeRank = rank - offset space

    coordinate stride size =
      (relativeRank `div` stride) `mod` size

spaceExtent :: AffineRankSpace -> Int
spaceExtent space =
  product (sizes space)
