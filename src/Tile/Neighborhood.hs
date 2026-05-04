module Tile.Neighborhood
  ( neighbors,
  )
where

import Tile.Affine
import Tile.Shape

neighbors :: AffineRankSpace -> Int -> [Int]
neighbors rankSpace rank =
  let point = pointOf rankSpace rank
   in [ rankOf rankSpace p'
      | p' <- neighborPoints (sizes rankSpace) point
      ]

neighborPoints :: Shape -> Point -> [Point]
neighborPoints shape point =
  concat
    [ moves dim
    | dim <- [0 .. length shape - 1]
    ]
  where
    moves dim =
      let x = point !! dim
          lo = replace dim (x - 1) point
          hi = replace dim (x + 1) point
          extent = shape !! dim
       in [ p
          | (v, p) <- [(x - 1, lo), (x + 1, hi)],
            v >= 0,
            v < extent
          ]

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs
