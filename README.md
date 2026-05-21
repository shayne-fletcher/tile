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

`BlockPartitioning` splits on one full dimension at a time. `Bisection` halves
the first non-singleton dimension, producing a balanced binary tree. Both
implement `Tiling`; `Tile.Tree.contractAnchors` derives the hop tree by
contracting anchor edges.

`BoundedFanout k` treats fan-out as a tiling policy. It keeps child tiles
affine rectangles while ensuring the hop tree exposes at most `k` immediate
communication children whenever `k` is at least the local geometric minimum.
If `k` is below that rectangular minimum, the tiler uses the minimum lawful
fan-out instead.

## Tree

Communication structure is materialized as trees.

`DecompositionTree`, `HopTree`, `SendTree`, and `RoutedTree` are semantic views over a common tree shape.

## Schedule

A `Schedule` is a directed communication plan derived from a tiling by taking roots along the communication tree.

`reverseSchedule` turns a fan-out plan into a converge plan.

## Execution

The same schedule algebra has two readings.

`Tile.Execution` gives pure reference semantics:

- `broadcastResult`
- `scatterResult`
- `gatherResult`
- `reduceResult`
- `allReduceResult`

`Tile.Execution.Concurrent` gives a small actor-style interpreter:

- `runBroadcast`
- `runScatter`
- `runGather`
- `runReduce`
- `runAllReduce`

The concurrent runners return observed results. The `run*WithTrace` variants
(`runBroadcastWithTrace`, `runReduceWithTrace`, `runGatherWithTrace`,
`runScatterWithTrace`, `runAllReduceWithTrace`) expose structured message-flow
events used by the demo.

`Tile.Collective` gives these operations a typed denotation through `Collective`,
`interpret`, and `runCollective`.

## Notes

The design is explained in [Structured Multicast via Affine Tiling](article/routing-foundations.md).

Bounded fan-out is described in [Bounded Fan-Out Tiling](article/bounded-fanout.md).

The correctness contract is stated in [Cast Routing Correctness via Affine Tiling](article/routing-theorems.md).
