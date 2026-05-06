module Tile.Schedule
  ( Step (..),
    Schedule,
    Scheduler (..),
    DFSScheduler (..),
    BFSScheduler (..),
    stepFor,
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

memberAt :: [a] -> Tile -> a
memberAt members  =(members !!) . root

stepFor :: [a] -> Tile -> Tile -> Maybe (Step a)
stepFor members parent child
  | root parent == root child = Nothing
  | otherwise =
      Just
        Step { from = memberAt members parent,
               to = memberAt members child
             }

class Scheduler s where
  buildScheduleFrom :: (Tiling t) => s -> t -> [a] -> Tile -> Schedule a

  buildSchedule :: (Tiling t) => s -> t -> [a] -> Shape -> Schedule a
  buildSchedule scheduler tiling members shp =
    buildScheduleFrom scheduler tiling members (rootTile shp)

data DFSScheduler = DFSScheduler
  deriving (Show, Eq)

instance Scheduler DFSScheduler where
  buildScheduleFrom _ tiling members =
    go
    where
      go tile =
        let childTiles = children tiling tile
            steps =
              [ step
              | child <- childTiles
              , Just step <- [stepFor members tile child]
              ]
         in steps ++ concatMap go childTiles

data BFSScheduler = BFSScheduler
  deriving (Show, Eq)

instance Scheduler BFSScheduler where
  buildScheduleFrom _ tiling members start =
    go [start]
    where
        go [] = []
        go tiles =
          let childTiles = concatMap (children tiling) tiles
              steps =
                [ step
                | parent <- tiles
                , child <- children tiling parent
                , Just step <- [stepFor members parent child]
                ]
           in steps ++ go childTiles

reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

reverseSchedule :: Schedule a -> Schedule a
reverseSchedule = map reverseStep
