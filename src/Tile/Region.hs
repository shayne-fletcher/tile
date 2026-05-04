module Tile.Region
  ( Region(..),
    contiguous,
    regionOrigin,
    regionExtent,
    regionRanks,
  ) where

import Tile.Range

data Region =
  Contiguous Range -- single-interval
  -- | Intervals [Range]  -- multi-interval
  -- | Sparse (Set Int) -- arbitrary mask (occlusion at single-rank granularity)
  deriving (Show, Eq)

contiguous :: Range -> Region
contiguous = Contiguous

regionOrigin :: Region -> Int
regionOrigin (Contiguous r) = start r

regionExtent :: Region -> Int
regionExtent (Contiguous r) = extent r

regionRanks :: Region -> [Int]
regionRanks (Contiguous (Range s e)) = [s .. s + e -1]
