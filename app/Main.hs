module Main where

import Tile

reverseStep :: Step a -> Step a
reverseStep Step {from = p, to = c} =
  Step {from = c, to = p}

reduceSchedule :: Schedule a -> Schedule a
reduceSchedule = map reverseStep

main :: IO ()
main = do
  let members = ["A", "B", "C", "D"]
      shape = [2, 2]

      tiling = BlockPartitioning
      scheduler = BFSScheduler

      broadcast = buildSchedule scheduler tiling members shape
      reduce = reduceSchedule broadcast

  putStrLn "broadcast schedule:"
  print broadcast

  putStrLn "\nreduce schedule:"
  print reduce

  putStrLn "\nrunning broadcast from A:"
  runBroadcast broadcast "A"

  putStrLn "\nrunning reduce:"
  runReduce
    reduce
    [ ("A", 1),
      ("B", 2),
      ("C", 3),
      ("D", 4)
    ]
    (+)
    "A"
