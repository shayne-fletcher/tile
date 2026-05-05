module Tile.Affine
  ( AffineRankSpace (..),
    Point,
    rowMajor,
    rankOf,
    pointOf,
    spaceExtent,
    select,
    fixDim
  )
where

import Control.Monad (guard)

import Tile.Shape (Shape)

data AffineRankSpace = AffineRankSpace
  { offset :: Int,
    sizes :: [Int],
    strides :: [Int]
  }
  deriving (Show, Eq)

type Point = [Int]

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

select :: AffineRankSpace -> Int -> Int -> Int -> Int -> Maybe AffineRankSpace
select space dim begin end step = do
  let shp = sizes space
      sts = strides space

  guard (dim >= 0)
  guard (dim < length shp)
  guard (step > 0)

  let extent = shp !! dim

  guard (begin >= 0)
  guard (begin < extent)
  guard (end > begin)
  guard (end <= extent)

  let newOffset = offset space + begin * (sts !! dim)
      newSize = (end - begin + step - 1) `div` step

  pure
    AffineRankSpace
      { offset = newOffset,
        sizes = replace dim newSize shp,
        strides = replace dim (sts !! dim * step) sts
      }
  where
    replace :: Int -> a -> [a] -> [a]
    replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

fixDim :: AffineRankSpace -> Int -> Int -> Maybe AffineRankSpace
fixDim space dim i = select space dim i (i + 1) 1
