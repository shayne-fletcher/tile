module Main where

import Data.Map.Strict qualified as Map
import Tile

main :: IO ()
main = do
  let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
      jaggedMembers = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
      shape = [2, 4]

      tiling = BlockPartitioning
      bounded = BoundedFanout 2
      bisection = Bisection

      schedule = buildSchedule BFS tiling members shape
      bisectionSchedule = buildSchedule BFS bisection members shape

      values =
        Map.fromList
          [ ("A", 1),
            ("B", 2),
            ("C", 3),
            ("D", 4),
            ("E", 5),
            ("F", 6),
            ("G", 7),
            ("H", 8)
          ]

      full = rootTile shape
      wide = rootTile [1, 8]
      bounded4 = BoundedFanout 4
      bounded3 = BoundedFanout 3
      occE = Occlusion (== "E")
      jaggedEnvelope = rootTile [3, 3]
      jaggedOcclusion = Occlusion (`elem` ["F", "H", "I"])
      lowerRightOcclusion = Occlusion (`elem` ["A", "B", "D"])

      repairedFull =
        expectRouted "occluded full mesh" $
          buildOccludedScheduleFrom BFS occE tiling members full
      jaggedRoute =
        expectRouted "jagged region" $
          buildOccludedScheduleFrom BFS jaggedOcclusion tiling jaggedMembers jaggedEnvelope
      lowerRightRoute =
        expectRouted "lower-right jagged region" $
          buildOccludedScheduleFrom BFS lowerRightOcclusion tiling jaggedMembers jaggedEnvelope

      row0 = expectTile "row 0" (Tile <$> fixDim (space full) 0 0)
      col0 = expectTile "column 0" (Tile <$> fixDim (space full) 1 0)
      middleColumns = expectTile "middle columns" (Tile <$> select (space full) 1 1 3 1)

      row0Schedule = buildScheduleFrom BFS tiling members row0
      col0Schedule = buildScheduleFrom BFS tiling members col0

  putStrLn "[2 x 4] decomposition tree:"
  putStr (renderDecompositionTree members (decompositionTree tiling full))

  putStrLn "\n[2 x 4] hop tree:"
  putStr (renderHopTree members (hopTree tiling full))

  putStrLn "\n[2 x 4] send tree:"
  putStr (renderSendTree members (sendTree tiling full))

  putStrLn "\n[2 x 4] bisection send tree:"
  putStr (renderSendTree members (sendTree bisection full))

  putStrLn "\n[1 x 8] block-partitioned send tree:"
  putStr (renderSendTree members (sendTree tiling wide))

  putStrLn "\n[1 x 8] bounded-fanout-2 send tree:"
  putStr (renderSendTree members (sendTree bounded wide))

  putStrLn "\n[1 x 8] bisection send tree:"
  putStr (renderSendTree members (sendTree bisection wide))

  putStrLn "\n[1 x 8] bounded-fanout-4 send tree:"
  putStr (renderSendTree members (sendTree bounded4 wide))

  putStrLn "\n[2 x 4] bounded-fanout-3 send tree:"
  putStr (renderSendTree members (sendTree bounded3 full))

  putStrLn "\n[2 x 4] bounded-fanout-4 send tree:"
  putStr (renderSendTree members (sendTree bounded4 full))

  putStrLn "\n[2 x 4] block-partitioned broadcast schedule:"
  print schedule

  putStrLn "\n[2 x 4] bisection broadcast schedule:"
  print bisectionSchedule

  putStrLn "\n[2 x 4] middle-columns tile ranks:"
  print (tileRanks middleColumns)

  putStrLn "\n[2 x 4] middle-columns decomposition tree:"
  putStr (renderDecompositionTree members (decompositionTree tiling middleColumns))

  putStrLn "\n[2 x 4] middle-columns hop tree:"
  putStr (renderHopTree members (hopTree tiling middleColumns))

  putStrLn "\n[2 x 4] middle-columns send tree:"
  putStr (renderSendTree members (sendTree tiling middleColumns))

  putStrLn "\n[2 x 4] schedule tree:"
  putStr (renderRoutedTree (scheduleTree "A" schedule))

  putStrLn "\n[2 x 4] row 0 ranks:"
  print (tileRanks row0)
  putStrLn "[2 x 4] row 0 broadcast schedule:"
  print row0Schedule

  putStrLn "\n[2 x 4] column 0 ranks:"
  print (tileRanks col0)
  putStrLn "[2 x 4] column 0 broadcast schedule:"
  print col0Schedule

  putStrLn "\n[2 x 4] row 0 broadcast from A:"
  runBroadcastWithTrace (putStrLn . renderTrace) row0Schedule "A" "hello"

  putStrLn "\n[2 x 4] column 0 broadcast from A:"
  runBroadcastWithTrace (putStrLn . renderTrace) col0Schedule "A" "hello"

  putStrLn "\n[2 x 4] broadcast from A:"
  runBroadcastWithTrace (putStrLn . renderTrace) schedule "A" "hello"

  putStrLn "\n[2 x 4] scatter from A:"
  runScatterWithTrace
    (putStrLn . renderTrace)
    schedule
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

  putStrLn "\n[2 x 4] occluded broadcast (E failed):"
  print repairedFull

  putStrLn "\n[2 x 4] occluded schedule tree:"
  putStr (renderRoutedTree (routedTree repairedFull))

  putStrLn "\n[3 x 3] jagged region {A B C D E G} via occluded envelope:"
  putStrLn "  A B C"
  putStrLn "  D E ."
  putStrLn "  G . ."
  print jaggedRoute

  putStrLn "\n[3 x 3] jagged routed tree:"
  putStr (renderRoutedTree (routedTree jaggedRoute))

  putStrLn "\n[3 x 3] lower-right jagged region {C E F G H I} via occluded envelope:"
  putStrLn "  . . C"
  putStrLn "  . E F"
  putStrLn "  G H I"
  print lowerRightRoute

  putStrLn "\n[3 x 3] lower-right jagged routed tree:"
  putStr (renderRoutedTree (routedTree lowerRightRoute))

  putStrLn "\n[2 x 4] occluded broadcast from ingress:"
  runBroadcastWithTrace (putStrLn . renderTrace) (routedSteps repairedFull) (ingress repairedFull) "hello"

  putStrLn "\n[2 x 4] reduce from A:"
  runReduceWithTrace (putStrLn . renderTrace) schedule values (+) "A"

  putStrLn "\n[2 x 4] gather from A:"
  _ <-
    runGatherWithTrace
      (putStrLn . renderTrace)
      schedule
      ( Map.fromList
          [ ("A", "value-a"),
            ("B", "value-b"),
            ("C", "value-c"),
            ("D", "value-d"),
            ("E", "value-e"),
            ("F", "value-f"),
            ("G", "value-g"),
            ("H", "value-h")
          ]
      )
      "A"

  putStrLn "\n[2 x 4] all-reduce from A:"
  result <- runAllReduceWithTrace (putStrLn . renderTrace) schedule "A" values (+)
  print result

expectTile :: String -> Maybe Tile -> Tile
expectTile _ (Just tile) = tile
expectTile label Nothing = error ("expected " ++ label)

expectRouted :: String -> Maybe (RoutedSchedule a) -> RoutedSchedule a
expectRouted _ (Just routed) = routed
expectRouted label Nothing = error ("expected " ++ label)

renderTrace :: (Show m, Show msg) => Trace m msg -> String
renderTrace (Received member msg) =
  show member ++ " received: " ++ show msg
renderTrace (Sent sender receiver msg) =
  show sender ++ " sent " ++ show msg ++ " to " ++ show receiver
renderTrace (Completed member msg) =
  show member ++ " completed: " ++ show msg
