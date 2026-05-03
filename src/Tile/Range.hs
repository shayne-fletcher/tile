module Tile.Range
  ( Range (..),
    end,
  )
where

data Range = Range
  { start :: Int,
    extent :: Int
  }
  deriving (Show, Eq, Ord)

end :: Range -> Int
end r = start r + extent r
