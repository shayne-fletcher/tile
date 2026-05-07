module Tile.Schedule
  ( Step (..),
    Schedule,
    RoutedSchedule (..),
    Scheduler (..),
    DFSScheduler (..),
    BFSScheduler (..),
    Occlusion (..),
    stepFor,
    stepForOccluded,
    reverseStep,
    reverseSchedule,
  )
where

import Data.List (find)
import Tile.Geometry
import Tile.Shape
import Tile.Tile
import Tile.Tiling

data Step a = Step
  { from :: a,
    to :: a
  }
  deriving (Show, Eq, Ord)

type Schedule a = [Step a]

data RoutedSchedule a = RoutedSchedule
  { ingress :: a,
    routedSteps :: Schedule a
  }
  deriving (Show, Eq)

newtype Occlusion a = Occlusion
  { isOccluded :: a -> Bool
  }

representative :: (Eq a) => Occlusion a -> [a] -> Tile -> Maybe a
representative occ members tile =
  find (not . isOccluded occ) [members !! r | r <- tileRanks tile]

memberAt :: [a] -> Tile -> a
memberAt members = (members !!) . root

stepFor :: [a] -> Tile -> Tile -> Maybe (Step a)
stepFor members parent child
  | root parent == root child = Nothing
  | otherwise =
      Just
        Step
          { from = memberAt members parent,
            to = memberAt members child
          }

stepForOccluded :: (Eq a) => Occlusion a -> [a] -> Tile -> Tile -> Maybe (Step a)
stepForOccluded occ members parent child =
  case (representative occ members parent, representative occ members child) of
    (Just fromMember, Just toMember)
      | fromMember /= toMember ->
          Just
            Step
              { from = fromMember,
                to = toMember
              }
    _ -> Nothing

class Scheduler s where
  buildScheduleFrom :: (Tiling t) => s -> t -> [a] -> Tile -> Schedule a
  buildOccludedScheduleFrom :: (Tiling t, Eq a) => s -> Occlusion a -> t -> [a] -> Tile -> Maybe (RoutedSchedule a)

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
              | child <- childTiles,
                Just step <- [stepFor members tile child]
              ]
         in steps ++ concatMap go childTiles

  buildOccludedScheduleFrom _ occ tiling members startTile = do
    startRep <- representative occ members startTile
    pure
      RoutedSchedule
        { ingress = startRep,
          routedSteps = go startTile
        }
    where
      liveChildren tile =
        [ child
        | child <- children tiling tile,
          case representative occ members child of
            Just _ -> True
            Nothing -> False
        ]

      go tile =
        let childTiles = liveChildren tile
            steps =
              [ step
              | child <- childTiles,
                Just step <- [stepForOccluded occ members tile child]
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
              | parent <- tiles,
                child <- children tiling parent,
                Just step <- [stepFor members parent child]
              ]
         in steps ++ go childTiles

  buildOccludedScheduleFrom _ occ tiling members start = do
    startRep <- representative occ members start
    pure
      RoutedSchedule
        { ingress = startRep,
          routedSteps = go [start]
        }
    where
      liveChildren tile =
        [ child
        | child <- children tiling tile,
          case representative occ members child of
            Just _ -> True
            Nothing -> False
        ]

      go [] = []
      go tiles =
        let frontier =
              [ (parent, liveChildren parent)
              | parent <- tiles
              ]
            childTiles = concatMap snd frontier
            steps =
              [ step
              | (parent, kids) <- frontier,
                child <- kids,
                Just step <- [stepForOccluded occ members parent child]
              ]
         in steps ++ go childTiles

reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

reverseSchedule :: Schedule a -> Schedule a
reverseSchedule = map reverseStep
