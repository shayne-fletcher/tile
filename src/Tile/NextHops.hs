module NextHops
  ( nextHops
  )
where

nextHops :: Tile -> [Tile]
nextHops tile = go (shape tile) (extent (range tile)) 0
  where
    go [] _ _ = []
    go (n : ns) blockLen offset0
    | n <= 1 || blockLen <= 1 = go ns blockLen offset0
    | otherwise =
      let childLen = blockLen `div` n
          children = [ subTile tile (offset0 + i * childLen) ns
                     | i <- [1 .. n - 1]
                     ]
      in children ++ go ns childLen offset0
