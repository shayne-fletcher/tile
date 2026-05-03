module Tile.Execution
  ( adjacencyList
  , runChanExecution
  ) where

import Tile.Schedule
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Control.Concurrent
import Control.Monad

adjacencyList :: Ord a => Schedule a -> Map.Map a [a]
adjacencyList =
  foldr (\Step { from = p, to = c } m -> Map.insertWith (++) p [c] m) Map.empty

runChanExecution :: Schedule String -> String -> IO ()
runChanExecution schedule root = do
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
