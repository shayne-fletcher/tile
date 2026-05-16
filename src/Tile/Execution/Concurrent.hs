-- |
-- Module      : Tile.Execution.Concurrent
-- Description : Actor-style concurrent interpreter for schedules.
--
-- These functions interpret schedules using lightweight Haskell
-- concurrency through channels and forked threads, and print the
-- resulting message flow. Their correctness contract is stated by the
-- pure functions in "Tile.Execution".
module Tile.Execution.Concurrent
  ( runBroadcast,
    runGather,
    runReduce,
    runScatter,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Tile.Schedule
import Tile.Tree (RoutedTree (..), scheduleTree, treeIndex, treeLabels)

-- | Run a broadcast schedule with the fixed message @"hello"@.
runBroadcast :: Schedule String -> String -> IO ()
runBroadcast schedule root = do
  let graph = adjacencyList schedule
      members =
        Set.toList $
          Set.fromList (Map.keys graph ++ concat (Map.elems graph))

  chans <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chans

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
    _ <- forkIO $ forever $ do
      msg <- readChan inbox
      putStrLn $ m ++ " received: " ++ msg
      forM_ childChans $ \(childName, childInbox) -> do
        putStrLn $ m ++ " forwarding to " ++ childName
        writeChan childInbox msg
    pure ()

  writeChan (chanMap Map.! root) "hello"
  threadDelay 1000000

incomingCounts :: (Ord a) => Schedule a -> Map.Map a Int
incomingCounts =
  foldr
    (\Step {to = c} m -> Map.insertWith (+) c 1 m)
    Map.empty

-- | Run a reduce schedule.
--
-- Leaf values flow toward the root and are combined at each node.
runReduce ::
  Schedule String ->
  [(String, Int)] ->
  (Int -> Int -> Int) ->
  String ->
  IO ()
runReduce schedule initialValues combine root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      members =
        Set.toList $
          Set.fromList $
            Map.keys graph ++ concat (Map.elems graph) ++ map fst initialValues

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs
      valueMap = Map.fromList initialValues

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
        expected = Map.findWithDefault 0 m incoming
        localValue = valueMap Map.! m

    _ <- forkIO $ do
      received <- replicateM expected (readChan inbox)
      let total = foldl' combine localValue received
      if m == root
        then putStrLn $ m ++ " reduced result: " ++ show total
        else forM_ childChans $ \(childName, childInbox) -> do
          putStrLn $ m ++ " sending reduced value " ++ show total ++ " to " ++ childName
          writeChan childInbox total
    pure ()

  forM_ members $ \m ->
    when (Map.findWithDefault 0 m incoming == 0) $
      writeChan (chanMap Map.! m) (valueMap Map.! m)

  threadDelay 1000000

-- | Run a gather schedule.
--
-- Each member contributes one value; values flow toward the root as
-- lists of member-value pairs.
runGather ::
  (Show a) =>
  Schedule String ->
  [(String, a)] ->
  String ->
  IO ()
runGather schedule initialValues root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      members =
        Set.toList $
          Set.fromList $
            Map.keys graph ++ concat (Map.elems graph) ++ map fst initialValues

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs
      valueMap = Map.fromList initialValues

  forM_ members $ \m -> do
    let inbox = chanMap Map.! m
        children = Map.findWithDefault [] m graph
        childChans = [(c, chanMap Map.! c) | c <- children]
        expected = Map.findWithDefault 0 m incoming
        localValue = [(m, valueMap Map.! m)]

    _ <- forkIO $ do
      received <- concat <$> replicateM expected (readChan inbox)
      let gathered = localValue ++ received
      if m == root
        then putStrLn $ m ++ " gathered result: " ++ show gathered
        else forM_ childChans $ \(childName, childInbox) -> do
          putStrLn $ m ++ " sending gathered values " ++ show gathered ++ " to " ++ childName
          writeChan childInbox gathered
    pure ()

  forM_ members $ \m ->
    when (Map.findWithDefault 0 m incoming == 0) $
      writeChan (chanMap Map.! m) [(m, valueMap Map.! m)]

  threadDelay 1000000

-- | Run a scatter schedule.
--
-- The root starts with a value for each destination. At each hop, the
-- payload is partitioned by the routed subtree below each child.
runScatter ::
  (Show a) =>
  Schedule String ->
  [(String, a)] ->
  String ->
  IO ()
runScatter schedule initialValues root = do
  let graph = adjacencyList schedule
      incoming = incomingCounts schedule
      members =
        Set.toList $
          Set.fromList $
            Map.keys graph ++ concat (Map.elems graph) ++ map fst initialValues
      RoutedTree routed = scheduleTree root schedule
      routedSubtrees = treeIndex routed

  chanPairs <- forM members $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chanPairs

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
      case lookup m payload of
        Just value -> putStrLn $ m ++ " received scatter value: " ++ show value
        Nothing -> pure ()

      forM_ childChans $ \(childName, childInbox) -> do
        let childMembers =
              maybe Set.empty treeLabels (Map.lookup childName routedSubtrees)
            childPayload =
              [ item
              | item@(dest, _) <- payload,
                dest `Set.member` childMembers
              ]
        putStrLn $ m ++ " scattering " ++ show childPayload ++ " to " ++ childName
        writeChan childInbox childPayload
    pure ()

  writeChan (chanMap Map.! root) initialValues
  threadDelay 1000000
