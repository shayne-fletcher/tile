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
    [ rangeTests,
      layoutTests,
      neighborTests,
      scheduleTests,
      affineTests
    ]

rangeTests :: TestTree
rangeTests =
  testGroup
    "range"
    [ testCase "end is exclusive" $
        end (Range 3 4) @?= 7,
      testCase "range fields" $ do
        let r = Range 3 4
        start r @?= 3
        extent r @?= 4
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
              ]
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
      testCase "pointOf rowMajor 2x3" $ do
        let space = rowMajor [2, 3]
        pointOf space 0 @?= [0, 0]
        pointOf space 1 @?= [0, 1]
        pointOf space 2 @?= [0, 2]
        pointOf space 3 @?= [1, 0]
        pointOf space 4 @?= [1, 1]
        pointOf space 5 @?= [1, 2],
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
