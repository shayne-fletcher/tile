-- |
-- Module      : Tile.Execution
-- Description : Pure execution semantics for schedules.
--
-- These functions interpret schedules as pure results. They provide a
-- denotational reference for concurrent interpreters.
module Tile.Execution
  ( -- * Pure execution semantics
    broadcastResult,
    reduceResult,
    gatherResult,
    scatterResult,
  )
where

import Data.Map.Strict qualified as Map
import Tile.Schedule
import Tile.Tree

-- | Delivery state after broadcast execution.
--
-- The result includes the root, which holds the payload from the
-- start. This records reachable members, not only receivers of
-- schedule steps.
broadcastResult :: (Ord m) => Schedule m -> m -> p -> Map.Map m p
broadcastResult schedule root payload =
  let RoutedTree tree = scheduleTree root schedule
   in Map.fromSet (const payload) (treeLabels tree)

-- | Result of reducing values along a schedule that flows toward the
-- root.
--
-- Precondition: the value map contains every member reachable from
-- the root.
reduceResult :: (Ord m) => Schedule m -> m -> Map.Map m v -> (v -> v -> v) -> v
reduceResult schedule root values combine =
  let RoutedTree tree = scheduleTree root (reverseSchedule schedule)
   in go tree
  where
    go (Tree member kids) =
      foldl' combine (values Map.! member) (map go kids)

-- | Result of gathering values at the root.
--
-- Values are returned in preorder over the reversed convergence tree.
--
-- Precondition: the value map contains every member reachable from
-- the root.
gatherResult :: (Ord m) => Schedule m -> m -> Map.Map m v -> [(m, v)]
gatherResult schedule root values =
  let RoutedTree tree = scheduleTree root (reverseSchedule schedule)
   in go tree
  where
    go (Tree member kids) =
      (member, values Map.! member) : concatMap go kids

-- | Result of scattering destination-specific payloads.
--
-- Only payloads whose destinations are reachable from the root are
-- delivered.
scatterResult :: (Ord m) => Schedule m -> m -> [(m, p)] -> Map.Map m p
scatterResult schedule root payloads =
  let RoutedTree tree = scheduleTree root schedule
      reachable = treeLabels tree
   in Map.restrictKeys (Map.fromList payloads) reachable
