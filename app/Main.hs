module Main where

import Tile

main :: IO ()
main = do
  let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
      jaggedMembers = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
      shape = [2, 4]

      tiling = BlockPartitioning
      scheduler = BFSScheduler

      broadcast = buildSchedule scheduler tiling members shape
      converge = reverseSchedule broadcast

      full = rootTile shape
      occE = Occlusion (== "E")
      jaggedEnvelope = rootTile [3, 3]
      jaggedOcclusion = Occlusion (`elem` ["F", "H", "I"])
      lowerRightOcclusion = Occlusion (`elem` ["A", "B", "D"])

      repairedFull =
        expectRouted "occluded full mesh" $
          buildOccludedScheduleFrom scheduler occE tiling members full
      jaggedRoute =
        expectRouted "jagged region" $
          buildOccludedScheduleFrom scheduler jaggedOcclusion tiling jaggedMembers jaggedEnvelope
      lowerRightRoute =
        expectRouted "lower-right jagged region" $
          buildOccludedScheduleFrom scheduler lowerRightOcclusion tiling jaggedMembers jaggedEnvelope

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

  putStrLn "\nmiddle-columns decomposition tree:"
  putStr (renderDecompositionTree members (decompositionTree tiling middleColumns))

  putStrLn "\nmiddle-columns hop tree:"
  putStr (renderHopTree members (hopTree tiling middleColumns))

  putStrLn "\nmiddle-columns send tree:"
  putStr (renderSendTree members (sendTree tiling middleColumns))

  putStrLn "\nschedule tree:"
  putStr (renderRoutedTree (scheduleTree "A" broadcast))

  putStrLn "\nrow 0 ranks:"
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

  putStrLn "\nrunning full-mesh scatter from A:"
  runScatter
    broadcast
    [ ("A", "payload-a"),
      ("B", "payload-b"),
      ("C", "payload-c"),
      ("D", "payload-d"),
      ("E", "payload-e"),
      ("F", "payload-f"),
      ("G", "payload-g"),
      ("H", "payload-h")
    ]
    "A"

  putStrLn "\noccluded full-mesh broadcast (E failed):"
  print repairedFull

  putStrLn "\noccluded schedule tree:"
  putStr (renderRoutedTree (routedTree repairedFull))

  putStrLn "\njagged region via occluded 3x3 envelope:"
  putStrLn "  A B C"
  putStrLn "  D E ."
  putStrLn "  G . ."
  print jaggedRoute

  putStrLn "\njagged routed tree:"
  putStr (renderRoutedTree (routedTree jaggedRoute))

  putStrLn "\nlower-right jagged region via occluded 3x3 envelope:"
  putStrLn "  . . C"
  putStrLn "  . E F"
  putStrLn "  G H I"
  print lowerRightRoute

  putStrLn "\nlower-right jagged routed tree:"
  putStr (renderRoutedTree (routedTree lowerRightRoute))

  putStrLn "\nrunning occluded full-mesh broadcast:"
  runBroadcast (routedSteps repairedFull) (ingress repairedFull)

  putStrLn "\nrunning full-mesh reduce:"
  runReduce
    converge
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

  putStrLn "\nrunning full-mesh gather:"
  runGather
    converge
    [ ("A", "value-a"),
      ("B", "value-b"),
      ("C", "value-c"),
      ("D", "value-d"),
      ("E", "value-e"),
      ("F", "value-f"),
      ("G", "value-g"),
      ("H", "value-h")
    ]
    "A"

expectTile :: String -> Maybe Tile -> Tile
expectTile _ (Just tile) = tile
expectTile label Nothing = error ("expected " ++ label)

expectRouted :: String -> Maybe (RoutedSchedule a) -> RoutedSchedule a
expectRouted _ (Just routed) = routed
expectRouted label Nothing = error ("expected " ++ label)
