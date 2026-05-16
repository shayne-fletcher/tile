-- |
-- Module      : Tile.Affine
-- Description : Affine rank spaces and slicing.
--
-- An affine rank space maps logical coordinates to ranks using an
-- offset and per-dimension strides. Slicing operations preserve the
-- affine representation.
module Tile.Affine
  ( -- * Affine rank spaces
    AffineRankSpace (..),
    Point,
    rowMajor,

    -- * Coordinate/rank conversion
    rankOf,
    rankOfMaybe,
    pointOf,
    pointOfMaybe,

    -- * Queries
    spaceExtent,
    ranks,

    -- * Slicing
    select,
    fixDim,
  )
where

import Control.Monad (guard)
import Tile.Shape (Shape)

-- | An affine map from logical coordinates to ranks.
--
-- For a coordinate @coord@, the rank is:
--
-- @
-- offset + sum (zipWith (*) coord strides)
-- @
data AffineRankSpace = AffineRankSpace
  { -- | Rank of the origin coordinate.
    offset :: Int,
    -- | Extents, or shape, of the affine rank space.
    sizes :: [Int],
    -- | Rank stride for each logical dimension.
    strides :: [Int]
  }
  deriving (Show, Eq)

-- | A logical coordinate in an affine rank space.
type Point = [Int]

-- | Construct the default row-major affine rank space for a shape.
rowMajor :: Shape -> AffineRankSpace
rowMajor shape =
  AffineRankSpace
    { offset = 0,
      sizes = shape,
      strides = drop 1 (scanr (*) 1 shape)
    }

-- | Convert a coordinate to a rank.
--
-- Throws an error if the coordinate has the wrong dimension or is out
-- of bounds. Use 'rankOfMaybe' for a total variant.
rankOf :: AffineRankSpace -> Point -> Int
rankOf space coord =
  case rankOfMaybe space coord of
    Just rank -> rank
    Nothing -> error "rankOf: coordinate dimension mismatch"

-- | Convert a coordinate to a rank, returning 'Nothing' for invalid
-- coordinates.
rankOfMaybe :: AffineRankSpace -> Point -> Maybe Int
rankOfMaybe space coord
  | length coord == length (strides space) && and (zipWith inBounds coord (sizes space)) =
      Just (offset space + sum (zipWith (*) coord (strides space)))
  | otherwise = Nothing
  where
    inBounds coordinate size = coordinate >= 0 && coordinate < size

-- | Convert a rank to a coordinate.
--
-- Throws an error if the rank is outside the affine rank space. Use
-- 'pointOfMaybe' for a total variant.
pointOf :: AffineRankSpace -> Int -> Point
pointOf space rank =
  case pointOfMaybe space rank of
    Just point -> point
    Nothing -> error "pointOf: rank outside affine rank space"

-- | Convert a rank to a coordinate, returning 'Nothing' for ranks
-- outside the affine rank space.
pointOfMaybe :: AffineRankSpace -> Int -> Maybe Point
pointOfMaybe space rank
  | rank `elem` ranks space =
      Just (zipWith coordinate (strides space) (sizes space))
  | otherwise = Nothing
  where
    relativeRank = rank - offset space

    coordinate stride size =
      (relativeRank `div` stride) `mod` size

-- | Number of logical points in an affine rank space.
spaceExtent :: AffineRankSpace -> Int
spaceExtent space =
  product (sizes space)

-- | Select a strided interval along one dimension.
--
-- The selected dimension remains present with a reduced extent.
-- Returns 'Nothing' for an invalid dimension, interval, or step.
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

-- | Select one index along a dimension.
--
-- The fixed dimension remains present with extent @1@.
fixDim :: AffineRankSpace -> Int -> Int -> Maybe AffineRankSpace
fixDim space dim i = select space dim i (i + 1) 1

-- | Enumerate all ranks in logical coordinate order.
ranks :: AffineRankSpace -> [Int]
ranks space = map (rankOf space) (points (sizes space))
  where
    points [] = [[]]
    points (n : ns) =
      [ i : rest
      | i <- [0 .. n - 1],
        rest <- points ns
      ]
