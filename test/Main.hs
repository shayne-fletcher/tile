module Main (main) where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "tile"
    [ layoutTests,
      neighborTests,
      selectTests,
      theoremTests,
      inclusionTests,
      treeTests,
      tilingTests,
      scheduleTests,
      affineTests
    ]

layoutTests :: TestTree
layoutTests =
  testGroup
    "layout"
    [ testCase "row-major pointOfRank 2x2x2" $
        pointOf (rowMajor [2, 2, 2]) 6 @?= [1, 1, 0],
      testCase "row-major rankOfPoint 2x2x2" $
        rankOf (rowMajor [2, 2, 2]) [1, 1, 0] @?= 6,
      testCase "row-major roundtrip ranks" $
        let space = rowMajor shape
         in [ rankOf space (pointOf space r)
            | r <- [0 .. size shape - 1]
            ]
              @?= [0 .. size shape - 1]
    ]
  where
    shape = [2, 2, 2]

neighborTests :: TestTree
neighborTests =
  testGroup
    "neighbors"
    [ testCase "neighbors rank 0 in 2x2" $
        neighbors (rowMajor [2, 2]) 0 @?= [2, 1],
      testCase "neighbors rank 3 in 2x2" $
        neighbors (rowMajor [2, 2]) 3 @?= [1, 2],
      testCase "neighbors center-ish rank 3 in 2x2x2" $
        neighbors (rowMajor [2, 2, 2]) 3 @?= [7, 1, 2]
    ]

theoremTests :: TestTree
theoremTests =
  testGroup
    "theorems"
    [ testProperty "T1 affine rank/point roundtrip" propAffineRoundtrip,
      testProperty "T2 ranks enumerate the affine space exactly once" propRanksEnumerateSpace,
      testProperty "T3 affine slicing is closed and included in its parent" propAffineSliceIncluded,
      testProperty "T4 structural and communication children are included in their parent" propChildrenIncluded,
      testProperty "T5 fault-free schedules form a spanning send tree" propFaultFreeScheduleSpansTile,
      testProperty "T6 occluded schedules deliver exactly to live members" propOccludedScheduleCoversLiveMembers
    ]

propAffineRoundtrip :: Property
propAffineRoundtrip =
  forAll genShape $ \shape ->
    let rankSpace = rowMajor shape
     in conjoin
          [ rankOf rankSpace (pointOf rankSpace rank) === rank
          | rank <- ranks rankSpace
          ]

propRanksEnumerateSpace :: Property
propRanksEnumerateSpace =
  forAll genShape $ \shape ->
    let rankSpace = rowMajor shape
        rankList = ranks rankSpace
     in conjoin
          [ length rankList === spaceExtent rankSpace,
            sort rankList === [0 .. spaceExtent rankSpace - 1]
          ]

propAffineSliceIncluded :: Property
propAffineSliceIncluded =
  forAll genAffineSlice $ \(shape, dim, begin, end, step) ->
    let parent = rowMajor shape
     in case select parent dim begin end step of
          Nothing -> counterexample "generated invalid affine slice" False
          Just child ->
            counterexample (show child) $
              all (`elem` ranks parent) (ranks child)

propChildrenIncluded :: Property
propChildrenIncluded =
  forAll genShape $ \shape ->
    let parent = rootTile shape
        structuralChildren = map tile (childNodes BlockPartitioning parent)
        communicationChildren = children BlockPartitioning parent
     in conjoin
          [ counterexample "structural child outside parent" $
              all (ranksIncludedIn parent) structuralChildren,
            counterexample "communication child outside parent" $
              all (ranksIncludedIn parent) communicationChildren
          ]

propFaultFreeScheduleSpansTile :: Property
propFaultFreeScheduleSpansTile =
  forAll genShape $ \shape ->
    let tile = rootTile shape
        memberRanks = tileRanks tile
        members = memberRanks
        schedule = buildScheduleFrom BFSScheduler BlockPartitioning members tile
        senders = map from schedule
        receivers = map to schedule
     in conjoin
          [ length schedule === length memberRanks - 1,
            sort receivers === sort (filter (/= root tile) memberRanks),
            unique receivers === True,
            all (`elem` memberRanks) senders === True,
            all (`elem` memberRanks) receivers === True,
            (root tile `notElem` receivers) === True
          ]

propOccludedScheduleCoversLiveMembers :: Property
propOccludedScheduleCoversLiveMembers =
  forAll genLiveRanks $ \(shape, liveRanks) ->
    let tile = rootTile shape
        memberRanks = tileRanks tile
        members = memberRanks
        live rank = rank `elem` liveRanks
        occ = Occlusion (not . live)
     in case buildOccludedScheduleFrom BFSScheduler occ BlockPartitioning members tile of
          Nothing ->
            counterexample "non-empty live set produced no schedule" $
              null liveRanks
          Just RoutedSchedule {ingress = entry, routedSteps = steps} ->
            let senders = map from steps
                receivers = map to steps
             in conjoin
                  [ counterexample "ingress is not live" $
                      live entry === True,
                    counterexample "sender outside live set" $
                      all live senders === True,
                    counterexample "receiver outside live set" $
                      all live receivers === True,
                    counterexample "live receiver coverage mismatch" $
                      sort receivers === sort (filter (/= entry) liveRanks),
                    counterexample "duplicate live receiver" $
                      unique receivers === True,
                    counterexample "receiver outside original tile" $
                      all (`elem` memberRanks) receivers === True
                  ]

genShape :: Gen Shape
genShape = do
  rank <- chooseInt (1, 4)
  vectorOf rank (chooseInt (1, 4))

genAffineSlice :: Gen (Shape, Int, Int, Int, Int)
genAffineSlice = do
  shape <- genShape
  dim <- chooseInt (0, length shape - 1)
  let extent = shape !! dim
  begin <- chooseInt (0, extent - 1)
  end <- chooseInt (begin + 1, extent)
  step <- chooseInt (1, extent)
  pure (shape, dim, begin, end, step)

genLiveRanks :: Gen (Shape, [Int])
genLiveRanks = do
  shape <- genShape
  let rankList = ranks (rowMajor shape)
  keep <- vectorOf (length rankList) arbitrary
  let liveRanks = [rank | (rank, True) <- zip rankList keep]
  pure (shape, liveRanks)

inclusionTests :: TestTree
inclusionTests =
  testGroup
    "inclusion"
    [ testCase "selected tiles include only ranks from the parent tile" $ do
        let full = rootTile [2, 4]
            row0 = expectTile "row 0" (Tile <$> fixDim (space full) 0 0)
            col1 = expectTile "column 1" (Tile <$> fixDim (space full) 1 1)
            middleColumns = expectTile "middle columns" (Tile <$> select (space full) 1 1 3 1)
        assertRanksIncludedIn full row0
        assertRanksIncludedIn full col1
        assertRanksIncludedIn full middleColumns,
      testCase "structural child nodes include only ranks from the parent tile" $ do
        let full = rootTile [2, 4]
            middleColumns = expectTile "middle columns" (Tile <$> select (space full) 1 1 3 1)
        map (tileRanks . tile) (childNodes BlockPartitioning middleColumns)
          @?= [[5, 6], [1, 2]]
        mapM_ (assertRanksIncludedIn middleColumns . tile) (childNodes BlockPartitioning middleColumns),
      testCase "communication children include only ranks from the parent tile" $ do
        let full = rootTile [2, 4]
            middleColumns = expectTile "middle columns" (Tile <$> select (space full) 1 1 3 1)
        map tileRanks (children BlockPartitioning middleColumns)
          @?= [[5, 6], [2]]
        mapM_ (assertRanksIncludedIn middleColumns) (children BlockPartitioning middleColumns),
      testCase "occluded schedule over jagged region sends only to live members" $ do
        let members = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
            occ = Occlusion (`elem` ["F", "H", "I"])
        buildOccludedScheduleFrom BFSScheduler occ BlockPartitioning members (rootTile [3, 3])
          @?= Just
            RoutedSchedule
              { ingress = "A",
                routedSteps =
                  [ Step "A" "D",
                    Step "A" "G",
                    Step "A" "B",
                    Step "A" "C",
                    Step "D" "E"
                  ]
              },
      testCase "occluded schedule over lower-right jagged region shifts ingress" $ do
        let members = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
            occ = Occlusion (`elem` ["A", "B", "D"])
        buildOccludedScheduleFrom BFSScheduler occ BlockPartitioning members (rootTile [3, 3])
          @?= Just
            RoutedSchedule
              { ingress = "C",
                routedSteps =
                  [ Step "C" "E",
                    Step "C" "G",
                    Step "E" "F",
                    Step "G" "H",
                    Step "G" "I"
                  ]
              }
    ]

treeTests :: TestTree
treeTests =
  testGroup
    "tree"
    [ testCase "mapTree maps every label and preserves shape" $
        mapTree (+ 1) sampleTree
          @?= Tree
            { treeLabel = 1,
              subtrees =
                [ Tree
                    { treeLabel = 2,
                      subtrees = []
                    },
                  Tree
                    { treeLabel = 3,
                      subtrees =
                        [ Tree
                            { treeLabel = 4,
                              subtrees = []
                            }
                        ]
                    }
                ]
            },
      testCase "renderTreeWith renders branch structure" $
        renderTreeWith show sampleTree
          @?= unlines
            [ "0",
              "├─ 1",
              "└─ 2",
              "   └─ 3"
            ],
      testCase "unfoldTree builds a tree from a child function" $
        unfoldTree (\n -> [n + 1 | n < 2]) (0 :: Int)
          @?= Tree
            { treeLabel = 0,
              subtrees =
                [ Tree
                    { treeLabel = 1,
                      subtrees =
                        [ Tree
                            { treeLabel = 2,
                              subtrees = []
                            }
                        ]
                    }
                ]
            }
    ]
  where
    sampleTree :: Tree Int
    sampleTree =
      Tree
        { treeLabel = 0,
          subtrees =
            [ Tree
                { treeLabel = 1,
                  subtrees = []
                },
              Tree
                { treeLabel = 2,
                  subtrees =
                    [ Tree
                        { treeLabel = 3,
                          subtrees = []
                        }
                    ]
                }
            ]
        }

tilingTests :: TestTree
tilingTests =
  testGroup
    "tiling"
    [ testCase "childNodes on 2x2 exposes sibling and anchor" $ do
        let full = rootTile [2, 2]
            nodes = childNodes BlockPartitioning full
        map relation nodes
          @?= [ Sibling (Split 0 1),
                Anchor (Split 0 0)
              ]
        map (tileRanks . tile) nodes
          @?= [ [2, 3],
                [0, 1]
              ],
      testCase "children on 2x2 keeps communication projection" $
        let full = rootTile [2, 2]
         in map tileRanks (children BlockPartitioning full)
              @?= [[2, 3], [1]],
      testCase "childNodes on top row exposes column split" $ do
        let full = rootTile [2, 2]
        case Tile <$> fixDim (space full) 0 0 of
          Nothing -> assertFailure "expected row tile"
          Just row0 -> do
            let nodes = childNodes BlockPartitioning row0
            map relation nodes
              @?= [ Sibling (Split 1 1),
                    Anchor (Split 1 0)
                  ]
            map (tileRanks . tile) nodes
              @?= [ [1],
                    [0]
                  ]
    ]

scheduleTests :: TestTree
scheduleTests =
  testGroup
    "schedule"
    [ testCase "2x2 schedule" $
        buildSchedule DFSScheduler BlockPartitioning ["A", "B", "C", "D"] [2, 2]
          @?= [ Step "A" "C",
                Step "A" "B",
                Step "C" "D"
              ],
      testCase "2x3 schedule" $
        buildSchedule DFSScheduler BlockPartitioning ["A", "B", "C", "D", "E", "F"] [2, 3]
          @?= [ Step "A" "D",
                Step "A" "B",
                Step "A" "C",
                Step "D" "E",
                Step "D" "F"
              ],
      testCase "schedule from row tile" $ do
        let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
            full = rootTile [2, 4]
        case Tile <$> fixDim (space full) 0 0 of
          Nothing -> assertFailure "expected row tile"
          Just row0 ->
            buildScheduleFrom BFSScheduler BlockPartitioning members row0
              @?= [ Step "A" "B",
                    Step "A" "C",
                    Step "A" "D"
                  ],
      testCase "schedule from column tile" $ do
        let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
            full = rootTile [2, 4]
        case Tile <$> fixDim (space full) 1 0 of
          Nothing -> assertFailure "expected column tile"
          Just col0 ->
            buildScheduleFrom BFSScheduler BlockPartitioning members col0
              @?= [Step "A" "E"],
      testCase "stepFor takes roots and ignores anchor self-edge" $ do
        let members = ["A", "B", "C", "D"]
            full = rootTile [2, 2]
        case childNodes BlockPartitioning full of
          [siblingNode, anchorNode] -> do
            relation siblingNode @?= Sibling (Split 0 1)
            relation anchorNode @?= Anchor (Split 0 0)
            stepFor members full (tile siblingNode)
              @?= Just (Step "A" "C")
            stepFor members full (tile anchorNode)
              @?= Nothing
          nodes ->
            assertFailure ("unexpected childNodes: " ++ show nodes),
      testCase "2x4 BFS schedule" $
        buildSchedule BFSScheduler BlockPartitioning ["A", "B", "C", "D", "E", "F", "G", "H"] [2, 4]
          @?= [ Step "A" "E",
                Step "A" "B",
                Step "A" "C",
                Step "A" "D",
                Step "E" "F",
                Step "E" "G",
                Step "E" "H"
              ],
      testCase "occluded DFS schedule reroots within subtree" $ do
        let members = ["A", "B", "C", "D"]
            occ = Occlusion (== "C")
        buildOccludedScheduleFrom DFSScheduler occ BlockPartitioning members (rootTile [2, 2])
          @?= Just
            RoutedSchedule
              { ingress = "A",
                routedSteps =
                  [ Step "A" "D",
                    Step "A" "B"
                  ]
              },
      testCase "occluded BFS schedule reroots bottom row" $ do
        let members = ["A", "B", "C", "D", "E", "F", "G", "H"]
            occ = Occlusion (== "E")
        buildOccludedScheduleFrom BFSScheduler occ BlockPartitioning members (rootTile [2, 4])
          @?= Just
            RoutedSchedule
              { ingress = "A",
                routedSteps =
                  [ Step "A" "F",
                    Step "A" "B",
                    Step "A" "C",
                    Step "A" "D",
                    Step "F" "G",
                    Step "F" "H"
                  ]
              }
    ]

affineTests :: TestTree
affineTests =
  testGroup
    "affine"
    [ testCase "rowMajor 2x3 fields" $ do
        let space = rowMajor [2, 3]
        offset space @?= 0
        sizes space @?= [2, 3]
        strides space @?= [3, 1]
        spaceExtent space @?= 6,
      testCase "rankOf rowMajor 2x3" $ do
        let space = rowMajor [2, 3]
        rankOf space [0, 0] @?= 0
        rankOf space [0, 1] @?= 1
        rankOf space [0, 2] @?= 2
        rankOf space [1, 0] @?= 3
        rankOf space [1, 1] @?= 4
        rankOf space [1, 2] @?= 5,
      testCase "rankOfMaybe rejects coordinate dimension mismatch" $ do
        let space = rowMajor [2, 3]
        rankOfMaybe space [1] @?= Nothing
        rankOfMaybe space [1, 2, 3] @?= Nothing,
      testCase "rankOfMaybe rejects out-of-bounds coordinates" $ do
        let space = rowMajor [2, 3]
        rankOfMaybe space [-1, 0] @?= Nothing
        rankOfMaybe space [2, 0] @?= Nothing
        rankOfMaybe space [0, 3] @?= Nothing,
      testCase "pointOf rowMajor 2x3" $ do
        let space = rowMajor [2, 3]
        pointOf space 0 @?= [0, 0]
        pointOf space 1 @?= [0, 1]
        pointOf space 2 @?= [0, 2]
        pointOf space 3 @?= [1, 0]
        pointOf space 4 @?= [1, 1]
        pointOf space 5 @?= [1, 2],
      testCase "pointOfMaybe rejects ranks outside the affine space" $ do
        let space = rowMajor [2, 3]
        pointOfMaybe space (-1) @?= Nothing
        pointOfMaybe space 6 @?= Nothing,
      testCase "rowMajor 2x2x2 roundtrip points" $ do
        let space = rowMajor [2, 2, 2]
            points =
              [ [0, 0, 0],
                [0, 0, 1],
                [0, 1, 0],
                [0, 1, 1],
                [1, 0, 0],
                [1, 0, 1],
                [1, 1, 0],
                [1, 1, 1]
              ]
        [pointOf space (rankOf space p) | p <- points] @?= points
    ]

{-
  - selection changes the extent of a dimension
  - it does not remove that dimension from the shape
  ```
  X =
  [
    [ A B ],
    [ C D ],
  ]
  select X 0 0 1 1 = [ [A, B] ] (1 x 2)
  select X 0 1 2 1 = [ [C, D] ] (1 x 2)

  select X 1 0 1 1 = [ [A], [C] ] (2 x 1)
  select X 1 1 2 1 = [ [B], [D] ] (2 x 1)
  ```
  so a 2 x 2 stays 2-dim after select
    - row selection gives 1 x 2
    - col selection gives 2 x 1

  ```
  Y =
  [
    [a, b, c, d],
    [e, f, g, h],
  ]
  select Y 1 1 3 1 =
  [
    [b, c],
    [f, g]
  ]
  ```
-}
selectTests :: TestTree
selectTests =
  testGroup
    "select"
    [ testCase "select row 0 keeps singleton dim" $
        select (rowMajor [2, 2]) 0 0 1 1
          @?= Just
            AffineRankSpace
              { offset = 0,
                sizes = [1, 2],
                strides = [2, 1]
              },
      testCase "select row 1 keeps singleton dim" $
        select (rowMajor [2, 2]) 0 1 2 1
          @?= Just
            AffineRankSpace
              { offset = 2,
                sizes = [1, 2],
                strides = [2, 1]
              },
      testCase "select column 0 keeps singleton dim" $
        select (rowMajor [2, 2]) 1 0 1 1
          @?= Just
            AffineRankSpace
              { offset = 0,
                sizes = [2, 1],
                strides = [2, 1]
              },
      testCase "select every other column" $
        select (rowMajor [2, 4]) 1 0 4 2
          @?= Just
            AffineRankSpace
              { offset = 0,
                sizes = [2, 2],
                strides = [4, 2]
              },
      testCase "ranks on every other column" $
        ranks <$> select (rowMajor [2, 4]) 1 0 4 2
          @?= Just [0, 2, 4, 6],
      testCase "select middle columns" $
        ranks <$> select (rowMajor [2, 4]) 1 1 3 1
          @?= Just [1, 2, 5, 6],
      testCase "select rejects empty range" $
        select (rowMajor [2, 2]) 1 1 1 1 @?= Nothing,
      testCase "select rejects negative dimension" $
        select (rowMajor [2, 2]) (-1) 0 1 1 @?= Nothing,
      testCase "select rejects out-of-bounds dimension" $
        select (rowMajor [2, 2]) 2 0 1 1 @?= Nothing,
      testCase "select rejects zero step" $
        select (rowMajor [2, 2]) 1 0 1 0 @?= Nothing,
      testCase "select rejects negative begin" $
        select (rowMajor [2, 2]) 1 (-1) 1 1 @?= Nothing,
      testCase "select rejects end beyond extent" $
        select (rowMajor [2, 2]) 1 0 3 1 @?= Nothing,
      testCase "fixDim intuition on [[0,1],[2,3]]" $ do
        let full = rowMajor [2, 2]
        ranks <$> fixDim full 0 0 @?= Just [0, 1]
        ranks <$> fixDim full 0 1 @?= Just [2, 3]
        ranks <$> fixDim full 1 0 @?= Just [0, 2]
        ranks <$> fixDim full 1 1 @?= Just [1, 3],
      testCase "fixDim keeps the selected dimension as size 1" $ do
        let full = rowMajor [2, 2]
        fixDim full 0 0
          @?= Just
            AffineRankSpace
              { offset = 0,
                sizes = [1, 2],
                strides = [2, 1]
              }
        fixDim full 1 0
          @?= Just
            AffineRankSpace
              { offset = 0,
                sizes = [2, 1],
                strides = [2, 1]
              }
    ]

assertRanksIncludedIn :: Tile -> Tile -> Assertion
assertRanksIncludedIn parent child =
  assertBool message (all (`elem` parentRanks) childRanks)
  where
    parentRanks = tileRanks parent
    childRanks = tileRanks child
    message =
      "expected "
        ++ show (sort childRanks)
        ++ " to be included in "
        ++ show (sort parentRanks)

expectTile :: String -> Maybe Tile -> Tile
expectTile _ (Just tile) = tile
expectTile description Nothing = error ("expected " ++ description)

ranksIncludedIn :: Tile -> Tile -> Bool
ranksIncludedIn parent child =
  all (`elem` tileRanks parent) (tileRanks child)

unique :: (Ord a) => [a] -> Bool
unique xs =
  sorted == dedupe sorted
  where
    sorted = sort xs

dedupe :: (Eq a) => [a] -> [a]
dedupe [] = []
dedupe [x] = [x]
dedupe (x : y : rest)
  | x == y = dedupe (y : rest)
  | otherwise = x : dedupe (y : rest)
