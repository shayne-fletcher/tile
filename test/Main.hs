module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
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
              @?= [Step "A" "E"]
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
