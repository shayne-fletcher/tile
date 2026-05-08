<p align="center">
  <img src="./images/logo.png" width="340" alt="tile logo">
</p>

<h1 align="center">tile</h1>

<p align="center">
  affine geometry for collective communication
</p>

<p align="center">
  <a href="https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml">
    <img src="https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml/badge.svg" alt="haskell ci">
  </a>
  <a href="https://shayne-fletcher.github.io/tile/">
    <img src="https://img.shields.io/badge/docs-github.io-blue" alt="docs">
  </a>
</p>

`tile` is a small Haskell model for structuring collective communication over affine views of a rank space.

> A tile is pure geometry.
> A schedule is communication.
> An executor gives the schedule meaning.

```text
Shape
  -> AffineRankSpace
    -> Tile
      -> Tiling
        -> Tree
          -> Schedule
            -> Execution
```

## AffineRankSpace

An N-dimensional rank space described by `offset`, `sizes`, and `strides`.

`select` and `fixDim` produce affine subspaces, so slices remain in the same representation.

## Tile

A `Tile` wraps an affine rank space.

Its root is the offset. Its members are the ranks covered by the affine view.

## Tiling

A `Tiling` decomposes a tile.

`BlockPartitioning` exposes the structural tree through `childNodes` and the communication projection through `nextHops`.

## Tree

Communication structure is materialized as trees.

`DecompositionTree`, `HopTree`, `SendTree`, and `RoutedTree` are semantic views over a common tree shape.

## Schedule

A `Schedule` is a directed communication plan derived from a tiling by taking roots along the communication tree.

`reverseSchedule` turns a fan-out plan into a converge plan.

## Execution

The same schedule algebra supports multiple runtimes:

- `runBroadcast`
- `runScatter`
- `runGather`
- `runReduce`

## Notes

The design is explained in [Structured Multicast via Affine Tiling](article/routing-foundations.md).

The correctness contract is stated in [Cast Routing Correctness via Affine Tiling](article/routing-theorems.md).
