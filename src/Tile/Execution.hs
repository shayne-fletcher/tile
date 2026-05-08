module Tile.Execution
  ( adjacencyList,
    runBroadcast,
    runGather,
    runReduce,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Tile.Schedule

adjacencyList :: (Ord a) => Schedule a -> Map.Map a [a]
adjacencyList =
  foldr (\Step {from = p, to = c} m -> Map.insertWith (++) p [c] m) Map.empty

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
      let total = foldl combine localValue received
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
