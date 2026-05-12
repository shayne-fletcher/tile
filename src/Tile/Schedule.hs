module Tile.Schedule
  ( Step (..),
    Schedule,
    RoutedSchedule (..),
    adjacencyList,
    Occlusion (..),
    representative,
    stepFor,
    stepForOccluded,
    reverseStep,
    reverseSchedule,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Tile.Geometry
import Tile.Tile

data Step a = Step
  { from :: a,
    to :: a
  }
  deriving (Show, Eq, Ord)

type Schedule a = [Step a]

adjacencyList :: (Ord a) => Schedule a -> Map.Map a [a]
adjacencyList =
  foldr
    (\Step {from = p, to = c} m -> Map.insertWith (++) p [c] m)
    Map.empty

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

reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

reverseSchedule :: Schedule a -> Schedule a
reverseSchedule = map reverseStep
