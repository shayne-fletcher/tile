module Tile.Schedule
  ( Step(..)
  , Schedule
  , Scheduler(..)
  , DFSScheduler(..)
  , BFSScheduler(..)
  ) where

import Tile.Range
import Tile.Shape
import Tile.Tile
import Tile.Tiling

data Step a = Step
  { from :: a
  , to :: a
  } deriving (Show, Eq, Ord)

type Schedule a = [Step a]

class Scheduler s where
  buildSchedule :: Tiling t => s -> t -> [a] -> Shape -> Schedule a

data DFSScheduler = DFSScheduler
  deriving (Show, Eq)

instance Scheduler DFSScheduler where
  buildSchedule _ tiling members shp =
    go (rootTile shp)
    where
      go tile =
        let parent = members !! start (range tile)
            childTiles = children tiling tile
            steps =
              [ Step
                { from = parent
                , to = members !! start (range child)
                }
              | child <- childTiles
              ]
        in steps ++ concatMap go childTiles

data BFSScheduler = BFSScheduler
  deriving (Show, Eq)

instance Scheduler BFSScheduler where
  buildSchedule _ tiling members shp =
    go [rootTile shp]
    where
      go [] = []
      go tiles =
        let childTiles = concatMap (children tiling) tiles
            steps =
              [ Step
                { from = members !! start (range parent)
                , to  = members !! start (range child)
                }
              | parent <- tiles
              , child <- children tiling parent
              ]
        in steps ++ go childTiles
