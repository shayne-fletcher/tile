module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Tile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tile"
    [ layoutTests
    , neighborTests
    ]

layoutTests :: TestTree
layoutTests =
  testGroup "layout"
    [ testCase "row-major pointOfRank 2x2x2" $
        pointOfRank RowMajor [2,2,2] 6 @?= [1,1,0]
    , testCase "row-major rankOfPoint 2x2x2" $
        rankOfPoint RowMajor [2,2,2] [1,1,0] @?= 6
    , testCase "row-major roundtrip ranks" $
        [ rankOfPoint RowMajor shape (pointOfRank RowMajor shape r)
        | r <- [0 .. size shape - 1]
        ] @?= [0 .. size shape - 1]
    ]
  where
    shape = [2,2,2]

neighborTests :: TestTree
neighborTests =
  testGroup "neighbors"
    [ testCase "neighbors rank 0 in 2x2" $
        neighbors RowMajor [2,2] 0 @?= [2,1]
    , testCase "neighbors rank 3 in 2x2" $
        neighbors RowMajor [2,2] 3 @?= [1,2]
    , testCase "neighbors center-ish rank 3 in 2x2x2" $
        neighbors RowMajor [2,2,2] 3 @?= [7,1,2]
    ]
