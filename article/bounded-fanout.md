# Bounded Fan-Out Tiling

`BlockPartitioning` is latency-oriented. It decomposes one dimension at a time
and gives a shallow communication tree, but the local fan-out can be large.

For a `1 x 8` tile:

```text
A B C D E F G H
```

block partitioning asks `A` to send directly to every other member:

```text
A
├─ B
├─ C
├─ D
├─ E
├─ F
├─ G
└─ H
```

`Bisection` is a low-fan-out reference point. It keeps local branching small
by making the tree deeper:

```text
A
├─ E
│  ├─ G
│  │  └─ H
│  └─ F
├─ C
│  └─ D
└─ B
```

`BoundedFanout k` exposes this tradeoff as a tiling parameter.

Large `k` gives shallow, block-partitioning-like schedules. Small feasible `k`
gives deeper, bisection-like schedules with lower local fan-out. The
interpolation is behavioral rather than structural: `BoundedFanout` computes a
local rectangular frontier, while `BlockPartitioning` and `Bisection` use
different decomposition shapes.

## Geometric Minimum

The fan-out cap is constrained by geometry.

With affine rectangular child tiles and a corner root, each active dimension
contributes one necessary frontier region away from the root. The minimum
lawful fan-out is therefore:

```haskell
minimumFanout tile =
  length [n | n <- sizes (space tile), n > 1]
```

For:

```text
A B
C D
```

the root is `A`. The non-root ranks separate into two necessary rectangular
frontier pieces:

```text
[B]
[C D]
```

There is no single affine rectangle covering `B C D` while excluding `A`.
Using one child would require either a jagged region, a child containing the
root, or an incomplete cover.

So:

```text
[4]       minimumFanout = 1
[2,2]     minimumFanout = 2
[2,2,2]   minimumFanout = 3
```

`BoundedFanout k` respects the requested cap when geometry permits it. If the
requested cap is below the rectangular minimum, the tiler uses the geometric
minimum instead:

```haskell
effectiveFanout tile k =
  max k (minimumFanout tile)
```

The core law is:

```haskell
length (children (BoundedFanout k) tile)
  <= effectiveFanout tile k
```

and when the requested cap is feasible:

```haskell
minimumFanout tile <= k
  ==> length (children (BoundedFanout k) tile) <= k
```

The fallback is local. A high-dimensional root may require more than the
requested cap, but interior tiles often have fewer active dimensions after
earlier splits. At those interior tiles, the requested cap is honored again.

## Relations

A tiling returns structural children as `TileNode`s. Each child has a
`Relation` to its parent:

```haskell
data Relation
  = Root
  | Anchor Split
  | Sibling Split
```

A `Sibling` has a distinct root and becomes a communication child. An `Anchor`
preserves the parent root. The tree layer derives the hop tree by contracting
anchor edges.

`BlockPartitioning` uses anchors recursively:

```text
parent
├─ sibling for dim 0
└─ anchor for dim 0
   ├─ sibling for dim 1
   └─ anchor for dim 1
```

Here anchors are load-bearing for decomposition: later dimensions are
discovered by recursively decomposing the anchor.

`BoundedFanout` uses the same relation algebra without modification:

```text
parent
├─ sibling frontier for dim 0
├─ sibling frontier for dim 1
└─ anchor root point
```

The anchor is terminal. It exists to preserve structural cover of the parent
tile, not to discover more communication children.

This is why the same `contractAnchors` operation works for both tilers:

- for `BlockPartitioning`, it removes recursive anchors and promotes later
  siblings;
- for `BoundedFanout`, it removes only the singleton root anchor, leaving the
  already-computed bounded frontier.

So `Relation` is not a block-partitioning artifact. It is the vocabulary that
separates structural cover from communication.

## Local Frontier

`BoundedFanout` keeps the rectangular affine model.

It does not introduce jagged regions, and it does not patch fan-out in the
scheduler. The tiler computes the full local frontier in one decomposition
step.

For active dimensions `d0`, `d1`, and `d2`, the frontier is:

```text
d0 away from root
d0 anchored, d1 away from root
d0 anchored, d1 anchored, d2 away from root
```

The remaining root point is a terminal anchor. Since that anchor has no
children, anchor contraction cannot promote additional communication nodes into
the hop frontier.

For `BoundedFanout 2` over `1 x 8`, the first hop is:

```text
[B C D E]
[F G H]
```

and the send tree is:

```text
A
├─ B
│  ├─ C
│  │  └─ D
│  └─ E
└─ F
   ├─ G
   └─ H
```

The root fan-out is bounded by `2`, and the depth increases accordingly.

## Interpretation

The tradeoff now lives in the tiling algebra:

```text
BlockPartitioning
  shallow, potentially high fan-out

Bisection
  low-fan-out reference point

BoundedFanout k
  tunable fan-out/depth tradeoff
```

The schedule and executor do not need special cases. They read the hop tree
produced by the tiler.

