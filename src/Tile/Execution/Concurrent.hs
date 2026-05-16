-- |
-- Module      : Tile.Execution.Concurrent
-- Description : Actor-style concurrent interpreter for schedules.
--
-- These functions interpret divergence schedules using lightweight
-- Haskell concurrency through channels and forked threads. A divergence
-- schedule has edges directed from root toward leaves; it is the form
-- produced by 'Tile.Routing.buildSchedule'. Collectives that require
-- leaf-to-root message flow (reduce, gather) derive the convergence
-- schedule internally.
--
-- The @run*@ forms return the observed result without tracing; the
-- @run*WithTrace@ forms also report structured 'Trace' events. Their
-- correctness contract is stated by the pure functions in
-- "Tile.Execution".
--
-- Precondition shared by all functions: the schedule must be rooted at
-- the supplied @root@. Passing a disconnected schedule may leave worker
-- threads waiting for messages that never arrive.
module Tile.Execution.Concurrent
  ( Trace (..),
    runBroadcast,
    runBroadcastWithTrace,
    runGather,
    runGatherWithTrace,
    runReduce,
    runReduceWithTrace,
    runScatter,
    runScatterWithTrace,
    runAllReduce,
    runAllReduceWithTrace,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Tile.Schedule
import Tile.Tree (RoutedTree (..), scheduleTree, treeIndex, treeLabels)

-- | An observed event in the concurrent schedule interpreter.
data Trace m msg
  = Received m msg
  | Sent m m msg
  | Completed m msg
  deriving (Show, Eq)

-- | Run a broadcast divergence schedule without tracing.
runBroadcast ::
  (Ord m) =>
  Schedule m ->
  m ->
  p ->
  IO (Map.Map m p)
runBroadcast =
  runBroadcastWithTrace (const (pure ()))

-- | Run a broadcast divergence schedule, reporting each observed action.
--
-- Precondition: the schedule is rooted at @root@.
runBroadcastWithTrace ::
  (Ord m) =>
  (Trace m p -> IO ()) ->
  Schedule m ->
  m ->
  p ->
  IO (Map.Map m p)
runBroadcastWithTrace trace schedule root payload = do
  let graph = adjacencyList schedule
      members =
        Set.toList $
          Set.fromList (root : Map.keys graph ++ concat (Map.elems graph))

  chans <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chans
  resultChan <- newChan

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
    _ <- forkIO $ do
      msg <- readChan inbox
      trace (Received m msg)
      writeChan resultChan (m, msg)
      forM_ childChans $ \(childName, childInbox) -> do
        trace (Sent m childName msg)
        writeChan childInbox msg
      trace (Completed m msg)
    pure ()

  writeChan (chanMap Map.! root) payload
  Map.fromList <$> replicateM (length members) (readChan resultChan)

incomingCounts :: (Ord a) => Schedule a -> Map.Map a Int
incomingCounts =
  foldr
    (\Step {to = c} m -> Map.insertWith (+) c 1 m)
    Map.empty

-- | Run a reduce divergence schedule without tracing.
--
-- Takes a divergence schedule. Leaf values flow toward the root and
-- are combined at each node in tree order, matching the fold order of
-- the pure 'Tile.Execution.reduceResult'. The combine function need
-- not be commutative.
runReduce ::
  (Ord m) =>
  Schedule m ->
  Map.Map m v ->
  (v -> v -> v) ->
  m ->
  IO v
runReduce =
  runReduceWithTrace (const (pure ()))

-- | Run a reduce divergence schedule, reporting each observed action.
--
-- Children are folded in the same order as the pure
-- 'Tile.Execution.reduceResult': left-to-right over the divergence
-- tree. Each directed edge gets a dedicated channel, so arrival order
-- does not affect the result.
--
-- Precondition: the schedule is rooted at @root@. The value map must
-- contain every member reachable from @root@.
runReduceWithTrace ::
  (Ord m) =>
  (Trace m v -> IO ()) ->
  Schedule m ->
  Map.Map m v ->
  (v -> v -> v) ->
  m ->
  IO v
runReduceWithTrace trace schedule initialValues combine root = do
  let childrenOf = adjacencyList schedule
      parentOf = Map.fromList [(child, parent) | Step {from = parent, to = child} <- schedule]
      members =
        Set.toList $
          Set.fromList $
            root : Map.keys childrenOf ++ concat (Map.elems childrenOf)

  -- One dedicated channel per directed edge (child → parent).
  edgeChans <- fmap Map.fromList $ forM schedule $ \Step {from = parent, to = child} -> do
    ch <- newChan
    pure ((child, parent), ch)

  result <- newEmptyMVar

  forM_ members $ \m -> do
    let myChildren = Map.findWithDefault [] m childrenOf
        localValue = initialValues Map.! m

    _ <- forkIO $ do
      childValues <- forM myChildren $ \child ->
        readChan (edgeChans Map.! (child, m))
      forM_ childValues $ \v -> trace (Received m v)
      let total = foldl' combine localValue childValues
      case Map.lookup m parentOf of
        Nothing -> do
          trace (Completed m total)
          putMVar result total
        Just parent -> do
          trace (Sent m parent total)
          writeChan (edgeChans Map.! (m, parent)) total
    pure ()

  takeMVar result

-- | Run a gather divergence schedule without tracing.
--
-- Takes a divergence schedule. Values are collected in preorder over
-- the divergence tree, matching the pure 'Tile.Execution.gatherResult'.
runGather ::
  (Ord m) =>
  Schedule m ->
  Map.Map m a ->
  m ->
  IO [(m, a)]
runGather =
  runGatherWithTrace (const (pure ()))

-- | Run a gather divergence schedule, reporting each observed action.
--
-- Values are accumulated in preorder over the divergence tree: each
-- node prepends its own value before appending children in tree order,
-- matching 'Tile.Execution.gatherResult'. Each directed edge gets a
-- dedicated channel so arrival order does not affect the result.
--
-- Precondition: the schedule is rooted at @root@. The value map must
-- contain every member reachable from @root@.
runGatherWithTrace ::
  (Ord m) =>
  (Trace m [(m, a)] -> IO ()) ->
  Schedule m ->
  Map.Map m a ->
  m ->
  IO [(m, a)]
runGatherWithTrace trace schedule initialValues root = do
  let childrenOf = adjacencyList schedule
      parentOf = Map.fromList [(child, parent) | Step {from = parent, to = child} <- schedule]
      members =
        Set.toList $
          Set.fromList $
            root : Map.keys childrenOf ++ concat (Map.elems childrenOf)

  edgeChans <- fmap Map.fromList $ forM schedule $ \Step {from = parent, to = child} -> do
    ch <- newChan
    pure ((child, parent), ch)

  result <- newEmptyMVar

  forM_ members $ \m -> do
    let myChildren = Map.findWithDefault [] m childrenOf
        localValue = [(m, initialValues Map.! m)]

    _ <- forkIO $ do
      childLists <- forM myChildren $ \child ->
        readChan (edgeChans Map.! (child, m))
      let allReceived = concat childLists
      unless (null allReceived) $
        trace (Received m allReceived)
      let gathered = localValue ++ allReceived
      case Map.lookup m parentOf of
        Nothing -> do
          trace (Completed m gathered)
          putMVar result gathered
        Just parent -> do
          trace (Sent m parent gathered)
          writeChan (edgeChans Map.! (m, parent)) gathered
    pure ()

  takeMVar result

-- | Run a scatter divergence schedule without tracing.
--
-- The root starts with a value for each destination. At each hop, the
-- payload is partitioned by the routed subtree below each child.
runScatter ::
  (Ord m) =>
  Schedule m ->
  [(m, a)] ->
  m ->
  IO (Map.Map m a)
runScatter =
  runScatterWithTrace (const (pure ()))

-- | Run a scatter divergence schedule, reporting each observed action.
--
-- Precondition: the schedule is rooted at @root@.
runScatterWithTrace ::
  (Ord m) =>
  (Trace m [(m, a)] -> IO ()) ->
  Schedule m ->
  [(m, a)] ->
  m ->
  IO (Map.Map m a)
runScatterWithTrace trace schedule initialValues root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      payloadMap = Map.fromList initialValues
      members =
        Set.toList $
          Set.fromList $
            root : Map.keys graph ++ concat (Map.elems graph) ++ Map.keys payloadMap
      RoutedTree routed = scheduleTree root schedule
      routedSubtrees = treeIndex routed
      reachablePayloads =
        Map.restrictKeys payloadMap (treeLabels routed)

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs
  resultChan <- newChan

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
        expected
          | m == root = 1
          | otherwise = Map.findWithDefault 0 m incoming

    _ <- forkIO $ do
      -- Scatter schedules are normally trees, so this usually reads
      -- one payload. For a general schedule, merge all incoming
      -- payload fragments before forwarding.
      payload <- concat <$> replicateM expected (readChan inbox)
      trace (Received m payload)
      forM_ (lookup m payload) $ \value ->
        writeChan resultChan (m, value)

      forM_ childChans $ \(childName, childInbox) -> do
        let childMembers =
              maybe Set.empty treeLabels (Map.lookup childName routedSubtrees)
            childPayload =
              [ item
              | item@(dest, _) <- payload,
                dest `Set.member` childMembers
              ]
        trace (Sent m childName childPayload)
        writeChan childInbox childPayload
      trace (Completed m payload)
    pure ()

  writeChan (chanMap Map.! root) (Map.toList payloadMap)
  Map.fromList <$> replicateM (Map.size reachablePayloads) (readChan resultChan)

-- | Run an all-reduce divergence schedule without tracing.
--
-- Takes a divergence schedule. Every member ends with the value
-- obtained by combining all member values with @combine@. Runs the
-- reduce phase to completion before starting the broadcast phase.
runAllReduce ::
  (Ord m) =>
  Schedule m ->
  m ->
  Map.Map m v ->
  (v -> v -> v) ->
  IO (Map.Map m v)
runAllReduce =
  runAllReduceWithTrace (const (pure ()))

-- | Run an all-reduce divergence schedule, reporting each observed
-- action.
--
-- Both the reduce phase and the broadcast phase emit 'Trace' events
-- through the same @tracer@. The reduce phase completes fully before
-- the broadcast phase begins.
--
-- Precondition: the schedule is rooted at @root@. The value map must
-- contain every member reachable from @root@.
runAllReduceWithTrace ::
  (Ord m) =>
  (Trace m v -> IO ()) ->
  Schedule m ->
  m ->
  Map.Map m v ->
  (v -> v -> v) ->
  IO (Map.Map m v)
runAllReduceWithTrace tracer schedule root values combine = do
  combined <- runReduceWithTrace tracer schedule values combine root
  runBroadcastWithTrace tracer schedule root combined
