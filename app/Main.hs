module Main where

import Tile
import Tile.Execution

main :: IO ()
main = do
  let members = ["A","B","C","D"]
      shape   = [2,2]

      tiling   = BlockPartitioning
      scheduler = DFSScheduler

      schedule = buildSchedule scheduler tiling members shape

  print schedule

  runChanExecution schedule "A"
