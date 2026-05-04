module Tile.Schedule
  ( Step (..),
    Schedule,
    Scheduler (..),
    DFSScheduler (..),
    BFSScheduler (..),
    reverseStep,
    reverseSchedule,
  )
where

import Tile.Shape
import Tile.Tile
import Tile.Tiling

data Step a = Step
  { from :: a,
    to :: a
  }
  deriving (Show, Eq, Ord)

type Schedule a = [Step a]

class Scheduler s where
  buildSchedule :: (Tiling t) => s -> t -> [a] -> Shape -> Schedule a

data DFSScheduler = DFSScheduler
  deriving (Show, Eq)

instance Scheduler DFSScheduler where
  buildSchedule _ tiling members shp =
    go (rootTile shp)
    where
      go tile =
        let parent = members !! root tile
            childTiles = children tiling tile
            steps =
              [ Step
                  { from = parent,
                    to = members !! root child
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
                  { from = members !! root parent,
                    to = members !! root child
                  }
              | parent <- tiles,
                child <- children tiling parent
              ]
         in steps ++ go childTiles

reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

reverseSchedule :: Schedule a -> Schedule a
reverseSchedule = map reverseStep
