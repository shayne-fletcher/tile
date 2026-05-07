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
      row0 = expectTile "row 0" (Tile <$> fixDim (space full) 0 0)
      col0 = expectTile "column 0" (Tile <$> fixDim (space full) 1 0)
      middleColumns = expectTile "middle columns" (Tile <$> select (space full) 1 1 3 1)

      row0Broadcast = buildScheduleFrom scheduler tiling members row0
      col0Broadcast = buildScheduleFrom scheduler tiling members col0

  putStrLn "decomposition tree:"
  putStr (renderDecompositionTree members (decompositionTree tiling full))

  putStrLn "\nhop tree:"
  putStr (renderHopTree members (hopTree tiling full))

  putStrLn "\nsend tree:"
  putStr (renderSendTree members (sendTree tiling full))

  putStrLn "\nmiddle-columns tile ranks:"
  print (tileRanks middleColumns)

  putStrLn "middle-columns decomposition tree:"
  putStr (renderDecompositionTree members (decompositionTree tiling middleColumns))

  putStrLn "\nmiddle-columns hop tree:"
  putStr (renderHopTree members (hopTree tiling middleColumns))

  putStrLn "\nmiddle-columns send tree:"
  putStr (renderSendTree members (sendTree tiling middleColumns))

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

expectTile :: String -> Maybe Tile -> Tile
expectTile _ (Just tile) = tile
expectTile label Nothing = error ("expected " ++ label)
