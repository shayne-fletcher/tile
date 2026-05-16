-- |
-- Module      : Tile
-- Description : Affine tiling for structured communication.
--
-- This umbrella module re-exports the public API for affine rank
-- spaces, tiles, tilings, tree views, schedules, routing, and
-- execution demos.
module Tile
  ( module Tile.Shape,
    module Tile.Tile,
    module Tile.Tiling,
    module Tile.Schedule,
    module Tile.Routing,
    module Tile.Execution,
    module Tile.Neighborhood,
    module Tile.Geometry,
    module Tile.Affine,
    module Tile.Tree,
  )
where

import Tile.Affine
import Tile.Execution
import Tile.Geometry
import Tile.Neighborhood
import Tile.Routing
import Tile.Schedule
import Tile.Shape
import Tile.Tile
import Tile.Tiling
import Tile.Tree
