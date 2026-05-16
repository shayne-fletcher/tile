-- |
-- Module      : Tile.Execution
-- Description : Pure execution semantics for schedules.
--
-- These functions interpret divergence schedules as pure results. A
-- divergence schedule has edges directed from root toward leaves; it is
-- the form produced by 'Tile.Routing.buildSchedule'. Each collective
-- derives convergence internally where the operation requires it.
--
-- These functions provide a denotational reference for the concurrent
-- interpreters in "Tile.Execution.Concurrent".
module Tile.Execution
  ( -- * Pure execution semantics
    broadcastResult,
    reduceResult,
    gatherResult,
    scatterResult,
    allReduceResult,
  )
where

import Data.Map.Strict qualified as Map
import Tile.Schedule
import Tile.Tree

-- | Deliver a payload from the root to every reachable member.
--
-- Takes a divergence schedule. The result includes the root, which
-- holds the payload from the start. This records reachable members,
-- not only receivers of schedule steps.
broadcastResult :: (Ord m) => Schedule m -> m -> p -> Map.Map m p
broadcastResult schedule root payload =
  let RoutedTree tree = scheduleTree root schedule
   in Map.fromSet (const payload) (treeLabels tree)

-- | Combine all member values into a single result at the root.
--
-- Takes a divergence schedule. Folds over the tree rooted at @root@,
-- combining each member's local value with those of its subtree.
--
-- Precondition: the value map contains every member reachable from
-- the root.
reduceResult :: (Ord m) => Schedule m -> m -> Map.Map m v -> (v -> v -> v) -> v
reduceResult schedule root values combine =
  let RoutedTree tree = scheduleTree root schedule
   in go tree
  where
    go (Tree member kids) =
      foldl' combine (values Map.! member) (map go kids)

-- | Collect all member values at the root as a list of pairs.
--
-- Takes a divergence schedule. Values are returned in preorder over
-- the divergence tree.
--
-- Precondition: the value map contains every member reachable from
-- the root.
gatherResult :: (Ord m) => Schedule m -> m -> Map.Map m v -> [(m, v)]
gatherResult schedule root values =
  let RoutedTree tree = scheduleTree root schedule
   in go tree
  where
    go (Tree member kids) =
      (member, values Map.! member) : concatMap go kids

-- | Deliver destination-specific payloads from the root.
--
-- Takes a divergence schedule. Only payloads whose destinations are
-- reachable from the root are delivered.
scatterResult :: (Ord m) => Schedule m -> m -> [(m, p)] -> Map.Map m p
scatterResult schedule root payloads =
  let RoutedTree tree = scheduleTree root schedule
      reachable = treeLabels tree
   in Map.restrictKeys (Map.fromList payloads) reachable

-- | Combine all member values and deliver the result to every member.
--
-- Takes a divergence schedule. Every reachable member ends with the
-- value obtained by combining all member values with @combine@.
--
-- Precondition: the value map contains every member reachable from
-- the root.
allReduceResult :: (Ord m) => Schedule m -> m -> Map.Map m v -> (v -> v -> v) -> Map.Map m v
allReduceResult schedule root values combine =
  broadcastResult schedule root combined
  where
    combined = reduceResult schedule root values combine
