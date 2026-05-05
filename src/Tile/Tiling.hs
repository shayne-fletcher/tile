{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Tile.Tiling
  ( Tiling (..),
    BlockPartitioning (..),
  )
where

import Tile.Affine
import Tile.Tile

class Tiling t where
  children :: t -> Tile -> [Tile]

data BlockPartitioning = BlockPartitioning
  deriving (Show, Eq)

fixTileDim :: Tile -> Int -> Int -> Maybe Tile
fixTileDim tile dim i = Tile <$> fixDim (space tile) dim i

instance Tiling BlockPartitioning where
  children _ tile = go tile 0
    where
      go t dim
        | dim >= length (sizes (space t)) = []
        | n <= 1 = go t (dim + 1)
        | otherwise =
            let siblings =
                  [ child
                  | i <- [1 .. n - 1]
                  , Just child <- [fixTileDim t dim i]
                  ]
                anchor =
                  case fixTileDim t dim 0 of
                    Just child -> child
                    Nothing -> error "impossible"
             in siblings ++ go anchor (dim + 1)
        where
          n = sizes (space t) !! dim
