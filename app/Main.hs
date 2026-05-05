module Main where

import Tile

main :: IO ()
main = do
  let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
      shape = [2, 4]

      tiling = BlockPartitioning
      scheduler = BFSScheduler

      broadcast = buildSchedule scheduler tiling members shape
      reduce = reverseSchedule broadcast

      full = rootTile shape
      Just row0 = Tile <$> fixDim (space full) 0 0
      Just col0 = Tile <$> fixDim (space full) 1 0

      row0Broadcast = buildScheduleFrom scheduler tiling members row0
      col0Broadcast = buildScheduleFrom scheduler tiling members col0

  putStrLn "row 0 ranks:"
  print (tileRanks row0)
  putStrLn "row 0 broadcast schedule:"
  print row0Broadcast

  putStrLn "\ncolumn 0 ranks:"
  print (tileRanks col0)
  putStrLn "column 0 broadcast schedule:"
  print col0Broadcast

  putStrLn "\nrunning row-0 broadcast from A:"
  runBroadcast row0Broadcast "A"

  putStrLn "\nrunning column-0 broadcast from A:"
  runBroadcast col0Broadcast "A"

  putStrLn "\nrunning full-mesh broadcast from A:"
  runBroadcast broadcast "A"

  putStrLn "\nrunning full-mesh reduce:"
  runReduce
    reduce
    [ ("A", 1),
      ("B", 2),
      ("C", 3),
      ("D", 4),
      ("E", 5),
      ("F", 6),
      ("G", 7),
      ("H", 8)
    ]
    (+)
    "A"
