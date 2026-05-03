{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tile.Tiling
  ( Tiling (..),
    BlockPartitioning (..),
  )
where

import Tile.Range
import Tile.Tile

class Tiling t where
  children :: t -> Tile -> [Tile]

data BlockPartitioning = BlockPartitioning
  deriving (Show, Eq)

instance Tiling BlockPartitioning where
  children _ tile =
    let Range _ blockLen = range tile
     in go (shape tile) blockLen 0
    where
      go [] _ _ = []
      go (n : ns) blockLen offset0
        | n <= 1 || blockLen <= 1 = go ns blockLen offset0
        | otherwise =
            let childLen = blockLen `div` n
                children =
                  [ subTile tile (offset0 + i * childLen) ns
                  | i <- [1 .. n - 1]
                  ]
             in children ++ go ns childLen offset0
