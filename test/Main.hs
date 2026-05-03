module Main (main) where

import Test.HUnit

import Tile

layoutTests :: Test
layoutTests =
  TestList
    [ "row-major pointOfRank 2x2x2" ~:
        pointOfRank RowMajor [2,2,2] 6 ~?= [1,1,0]
    , "row-major rankOfPoint 2x2x2" ~:
        rankOfPoint RowMajor [2,2,2] [1,1,0] ~?= 6
    , "row-major roundtrip ranks" ~:
        [ rankOfPoint RowMajor shape (pointOfRank RowMajor shape r)
        | r <- [0 .. size shape - 1]
        ] ~?= [0 .. size shape - 1]
    ]
  where
    shape = [2,2,2]

main :: IO Counts
main = runTestTT layoutTests
