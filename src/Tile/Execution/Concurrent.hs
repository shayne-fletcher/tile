-- |
-- Module      : Tile.Execution.Concurrent
-- Description : Actor-style concurrent interpreter for schedules.
--
-- These functions interpret schedules using lightweight Haskell
-- concurrency through channels and forked threads. The @run*@ forms
-- return the observed result without tracing; the @run*WithTrace@
-- forms also report structured trace events. Their correctness
-- contract is stated by the pure functions in "Tile.Execution".
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

-- | Run a broadcast schedule without tracing.
runBroadcast ::
  (Ord m) =>
  Schedule m ->
  m ->
  p ->
  IO (Map.Map m p)
runBroadcast =
  runBroadcastWithTrace (const (pure ()))

-- | Run a broadcast schedule, reporting each observed action.
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

-- | Run a reduce schedule without tracing.
--
-- Leaf values flow toward the root and are combined at each node.
runReduce ::
  (Ord m) =>
  Schedule m ->
  Map.Map m v ->
  (v -> v -> v) ->
  m ->
  IO v
runReduce =
  runReduceWithTrace (const (pure ()))

-- | Run a reduce schedule, reporting each observed action.
runReduceWithTrace ::
  (Ord m) =>
  (Trace m v -> IO ()) ->
  Schedule m ->
  Map.Map m v ->
  (v -> v -> v) ->
  m ->
  IO v
runReduceWithTrace trace schedule initialValues combine root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      members =
        Set.toList $
          Set.fromList $
            root : Map.keys graph ++ concat (Map.elems graph) ++ Map.keys initialValues

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs
  result <- newEmptyMVar

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
        expected = Map.findWithDefault 0 m incoming
        localValue = initialValues Map.! m

    _ <- forkIO $ do
      received <- replicateM expected (readChan inbox)
      forM_ received $ \value ->
        trace (Received m value)
      let total = foldl' combine localValue received
      if m == root
        then do
          trace (Completed m total)
          putMVar result total
        else forM_ childChans $ \(childName, childInbox) -> do
          trace (Sent m childName total)
          writeChan childInbox total
    pure ()

  takeMVar result

-- | Run a gather schedule without tracing.
--
-- Each member contributes one value; values flow toward the root as
-- lists of member-value pairs.
runGather ::
  (Ord m) =>
  Schedule m ->
  Map.Map m a ->
  m ->
  IO [(m, a)]
runGather =
  runGatherWithTrace (const (pure ()))

-- | Run a gather schedule, reporting each observed action.
runGatherWithTrace ::
  (Ord m) =>
  (Trace m [(m, a)] -> IO ()) ->
  Schedule m ->
  Map.Map m a ->
  m ->
  IO [(m, a)]
runGatherWithTrace trace schedule initialValues root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      members =
        Set.toList $
          Set.fromList $
            root : Map.keys graph ++ concat (Map.elems graph) ++ Map.keys initialValues

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs
  result <- newEmptyMVar

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
        expected = Map.findWithDefault 0 m incoming
        localValue = [(m, initialValues Map.! m)]

    _ <- forkIO $ do
      received <- concat <$> replicateM expected (readChan inbox)
      unless (null received) $
        trace (Received m received)
      let gathered = localValue ++ received
      if m == root
        then do
          trace (Completed m gathered)
          putMVar result gathered
        else forM_ childChans $ \(childName, childInbox) -> do
          trace (Sent m childName gathered)
          writeChan childInbox gathered
    pure ()

  takeMVar result

-- | Run a scatter schedule without tracing.
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

-- | Run a scatter schedule, reporting each observed action.
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
