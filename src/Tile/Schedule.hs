-- |
-- Module      : Tile.Schedule
-- Description : Schedule data and edge primitives.
--
-- A schedule is a list of directed send steps. This module defines
-- the schedule representation and the primitive functions that turn
-- tile relationships into member-to-member communication edges.
--
-- [Divergence schedule]
--   Edges run from root toward leaves. This is the form produced by
--   'Tile.Routing.buildSchedule' and accepted by every collective in
--   "Tile.Execution" and "Tile.Execution.Concurrent".
--
-- [Convergence schedule]
--   Edges run from leaves toward root. Obtained by applying
--   'reverseSchedule' to a divergence schedule. Execution APIs derive
--   convergence internally where needed; callers rarely need it
--   directly.
module Tile.Schedule
  ( -- * Schedule representation
    Step (..),
    Schedule,
    RoutedSchedule (..),
    adjacencyList,

    -- * Occlusion
    Occlusion (..),
    representative,

    -- * Step construction
    stepFor,
    stepForOccluded,

    -- * Reversal
    reverseStep,
    reverseSchedule,
  )
where

import Data.List (find)
import Data.Map.Strict qualified as Map
import Tile.Geometry
import Tile.Tile

-- | A directed communication step from one member to another.
data Step a = Step
  { -- | Sender.
    from :: a,
    -- | Receiver.
    to :: a
  }
  deriving (Show, Eq, Ord)

-- | A communication schedule: an ordered list of directed steps.
type Schedule a = [Step a]

-- | Group schedule steps by sender.
adjacencyList :: (Ord a) => Schedule a -> Map.Map a [a]
adjacencyList =
  foldr
    (\Step {from = p, to = c} m -> Map.insertWith (++) p [c] m)
    Map.empty

-- | A schedule with an explicit ingress member.
--
-- Under occlusion, the ingress may differ from the natural root of
-- the starting tile.
data RoutedSchedule a = RoutedSchedule
  { -- | Member where the routed schedule begins.
    ingress :: a,
    -- | Directed steps in the routed schedule.
    routedSteps :: Schedule a
  }
  deriving (Show, Eq)

-- | A predicate describing unavailable members.
newtype Occlusion a = Occlusion
  { -- | Return 'True' when a member is unavailable.
    isOccluded :: a -> Bool
  }

-- | Choose the first non-occluded member in a tile.
representative :: (Eq a) => Occlusion a -> [a] -> Tile -> Maybe a
representative occ members tile =
  find (not . isOccluded occ) [members !! r | r <- tileRanks tile]

memberAt :: [a] -> Tile -> a
memberAt members = (members !!) . root

-- | Build a fault-free step from a parent tile to a child tile.
--
-- Returns 'Nothing' when the two tiles have the same root, since that
-- would be a self-edge.
stepFor :: [a] -> Tile -> Tile -> Maybe (Step a)
stepFor members parent child
  | root parent == root child = Nothing
  | otherwise =
      Just
        Step
          { from = memberAt members parent,
            to = memberAt members child
          }

-- | Build a step using live representatives for the parent and child.
--
-- Returns 'Nothing' if either tile has no representative or if both
-- tiles choose the same representative.
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

-- | Reverse the direction of one step, converting a divergence edge to
-- a convergence edge or vice versa.
reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

-- | Convert a divergence schedule to a convergence schedule, or vice
-- versa, by reversing every step.
reverseSchedule :: Schedule a -> Schedule a
reverseSchedule = map reverseStep
