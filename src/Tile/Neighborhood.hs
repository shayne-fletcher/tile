module Tile.Neighborhood
  ( neighbors,
  )
where

import Tile.Layout
import Tile.Shape

neighbors :: (Layout l) => l -> Shape -> Int -> [Int]
neighbors layout shape rank =
  let point = pointOfRank layout shape rank
   in [ rankOfPoint layout shape p'
      | p' <- neighborPoints shape point
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
