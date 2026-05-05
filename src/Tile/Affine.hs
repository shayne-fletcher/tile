module Tile.Affine
  ( AffineRankSpace (..),
    Point,
    rowMajor,
    rankOf,
    rankOfMaybe,
    pointOf,
    pointOfMaybe,
    spaceExtent,
    select,
    fixDim,
    ranks,
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
      strides = drop 1 (scanr (*) 1 shape)
    }

rankOf :: AffineRankSpace -> Point -> Int
rankOf space coord =
  case rankOfMaybe space coord of
    Just rank -> rank
    Nothing -> error "rankOf: coordinate dimension mismatch"

rankOfMaybe :: AffineRankSpace -> Point -> Maybe Int
rankOfMaybe space coord
  | length coord == length (strides space) && and (zipWith inBounds coord (sizes space)) =
      Just (offset space + sum (zipWith (*) coord (strides space)))
  | otherwise = Nothing
  where
    inBounds coordinate size = coordinate >= 0 && coordinate < size

pointOf :: AffineRankSpace -> Int -> Point
pointOf space rank =
  case pointOfMaybe space rank of
    Just point -> point
    Nothing -> error "pointOf: rank outside affine rank space"

pointOfMaybe :: AffineRankSpace -> Int -> Maybe Point
pointOfMaybe space rank
  | rank `elem` ranks space =
      Just (zipWith coordinate (strides space) (sizes space))
  | otherwise = Nothing
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

ranks :: AffineRankSpace -> [Int]
ranks space = map (rankOf space) (points (sizes space))
  where
    points [] = [[]]
    points (n : ns) =
      [ i : rest
      | i <- [0 .. n - 1],
        rest <- points ns
      ]
