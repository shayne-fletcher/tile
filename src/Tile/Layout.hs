module Tile.Layout
  ( Point
  , Layout(..)
  , RowMajor(..)
  ) where

import Tile.Shape

type Point =  [Int]

class Layout l where
  pointOfRank :: l -> Shape -> Int -> Point
  rankOfPoint :: l -> Shape -> Point -> Int

data RowMajor = RowMajor
  deriving (Show, Eq)

instance Layout RowMajor where
   pointOfRank _ shape rank =
     let strides = tail (scanr (*) 1 shape)
     in zipWith (\stride dim -> (rank `div` stride) `mod` dim) strides shape

   rankOfPoint _ shape point =
     let strides = tail (scanr (*) 1 shape)
     in sum (zipWith (*) point strides)
