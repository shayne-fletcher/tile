{-# LANGUAGE GADTs #-}

-- |
-- Module      : Tile.Collective
-- Description : First-class denotable collective operations.
--
-- This module lifts each collective operation into a value of type
-- 'Collective' @m a b@, where @m@ is the member type, @a@ is the
-- element type, and @b@ is the result type.
--
-- == Input and output shapes
--
-- +-------------+----------------------+--------------------+
-- | Operation   | Input                | Output (@b@)       |
-- +=============+======================+====================+
-- | 'Broadcast' | one value (@a@)      | @Map m a@          |
-- | 'Scatter'   | @Map m a@            | @Map m a@          |
-- | 'Gather'    | @Map m a@            | @[(m, a)]@         |
-- | 'Reduce'    | @Map m a@, @a->a->a@ | @a@                |
-- | 'AllReduce' | @Map m a@, @a->a->a@ | @Map m a@          |
-- +-------------+----------------------+--------------------+
--
-- == Orientation duality
--
-- 'Broadcast' and 'Reduce' use opposite orientations of the same
-- routing tree. 'Scatter' and 'Gather' do the same for
-- destination-indexed payloads.
--
-- == Compositional law
--
-- 'AllReduce' decomposes as 'Reduce' followed by 'Broadcast':
--
-- @
-- interpret s r (AllReduce vals f)
--   == interpret s r (Broadcast (interpret s r (Reduce vals f)))
-- @
--
-- This identity is stated as a machine-checked property (T15).
module Tile.Collective
  ( -- * Collective type
    Collective (..),

    -- * Interpreters
    interpret,
    runCollective,
  )
where

import Data.Map.Strict qualified as Map
import Tile.Execution
  ( allReduceResult,
    broadcastResult,
    gatherResult,
    reduceResult,
    scatterResult,
  )
import Tile.Execution.Concurrent
  ( runAllReduce,
    runBroadcast,
    runGather,
    runReduce,
    runScatter,
  )
import Tile.Schedule (Schedule)

-- | A collective operation over members of type @m@, element type
-- @a@, and result type @b@.
--
-- Constructors embed all inputs except the schedule and root member,
-- which are supplied by 'interpret' and 'runCollective'.
data Collective m a b where
  -- | Deliver one value to every reachable member.
  Broadcast :: a -> Collective m a (Map.Map m a)
  -- | Deliver a per-member payload from the root. The map is
  -- destination-indexed; duplicate destinations cannot be
  -- represented, which matches scatter's semantics.
  Scatter :: Map.Map m a -> Collective m a (Map.Map m a)
  -- | Collect every member's value at the root as a list, in
  -- preorder over the routing tree.
  Gather :: Map.Map m a -> Collective m a [(m, a)]
  -- | Combine all member values into one at the root. The combine
  -- function is applied in tree order, left-to-right.
  Reduce :: Map.Map m a -> (a -> a -> a) -> Collective m a a
  -- | Combine all member values and deliver the result to every
  -- member. Precondition: the value map must contain every member
  -- reachable from the root.
  AllReduce :: Map.Map m a -> (a -> a -> a) -> Collective m a (Map.Map m a)

-- | Interpret a collective purely, delegating to 'Tile.Execution'.
--
-- Precondition: the schedule is rooted at @root@.
interpret :: (Ord m) => Schedule m -> m -> Collective m a b -> b
interpret schedule root collective = case collective of
  Broadcast payload ->
    broadcastResult schedule root payload
  Scatter vals ->
    scatterResult schedule root (Map.toList vals)
  Gather vals ->
    gatherResult schedule root vals
  Reduce vals combine ->
    reduceResult schedule root vals combine
  AllReduce vals combine ->
    allReduceResult schedule root vals combine

-- | Run a collective concurrently, delegating to
-- 'Tile.Execution.Concurrent'.
--
-- Presents a uniform @(schedule, root, collective)@ argument order,
-- hiding the positional inconsistencies in the underlying @run*@
-- functions.
--
-- Precondition: the schedule is rooted at @root@.
runCollective :: (Ord m) => Schedule m -> m -> Collective m a b -> IO b
runCollective schedule root collective = case collective of
  Broadcast payload ->
    runBroadcast schedule root payload
  Scatter vals ->
    runScatter schedule (Map.toList vals) root
  Gather vals ->
    runGather schedule vals root
  Reduce vals combine ->
    runReduce schedule vals combine root
  AllReduce vals combine ->
    runAllReduce schedule root vals combine
