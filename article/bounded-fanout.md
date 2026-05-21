# Bounded Fanout Tiling

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

Large `k` approaches `BlockPartitioning` at the root; interior tiles continue to
observe the cap as they recurse, so the per-hop fan-out budget is honored
throughout the tree rather than only at the entry point. Small feasible `k`
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

When the requested cap exceeds the per-dimension floor, the surplus is
distributed across active dimensions in declaration order, up to each
dimension's capacity (`n - 1` for a dimension of size `n`). So `BoundedFanout 4`
over a `2 x 4` tile gives one frontier piece to dimension `0` (capacity `1`,
saturated) and two frontier pieces to dimension `1` — the dim order matters
when the cap is between the floor and the maximum.

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

### Multi-dimensional frontier: `BoundedFanout 3` over `2 x 4`

For a `2 x 4` tile:

```text
A B C D
E F G H
```

dimension `0` has capacity `1` (size `2`), dimension `1` has capacity `3`
(size `4`). `minimumFanout = 2`; `effectiveFanout` with `k = 3` is `3`.
`allocateGroups` gives `[1, 2]` — one frontier piece for dim `0`, two for dim
`1`. The root frontier:

```text
[E F G H]    dim 0, away-from-root
[B C]        dim 1, with dim 0 anchored
[D]          dim 1, with dim 0 anchored
```

and the send tree:

```text
A
├─ E
│  ├─ F
│  ├─ G
│  └─ H
├─ B
│  └─ C
└─ D
```

Root fan-out is exactly `3`. At interior tile `E` (a `1 x 4` subtile),
`activeDims` shrinks to one, the per-dim floor drops to one, and the cap is
fully available — `E`'s fan-out is again `3`. At `B` (a `1 x 2` subtile)
geometry caps fan-out at `1`.

### Narrow rectangle with larger `k`: `BoundedFanout 4` over `1 x 8`

To show that `BoundedFanout` isn't just a relabelled bisection, raise `k`
above `2` on the same `1 x 8` tile. With `k = 4`, `allocateGroups` gives `[4]`,
`boundedIntervals 4 8` yields four intervals of sizes `[2, 2, 2, 1]`, and the
root frontier becomes:

```text
[B C]    [D E]    [F G]    [H]
```

The send tree:

```text
A
├─ B
│  └─ C
├─ D
│  └─ E
├─ F
│  └─ G
└─ H
```

Compare against bisection on the same `1 x 8` (fan-out 3, depth 3) and block
partitioning (fan-out 7, depth 1). `BoundedFanout 4` sits between them with
fan-out `4` and depth `2` — exactly the tunable point the parameter is meant
to expose. The intervals are sized by integer division (the first `extra =
remaining mod groups` intervals get one more element), so distribution is
deterministic and balanced.

## Interpretation

The tradeoff now lives in the tiling algebra:

```text
BlockPartitioning
  shallow, potentially high fan-out

Bisection
  low-fan-out reference point

BoundedFanout k
  tunable fan-out/depth tradeoff, honored at every tile in the tree
```

The schedule and executor do not need special cases. They read the hop tree
produced by the tiler.

## When to pick which

- **`BlockPartitioning`** when latency dominates and per-hop fan-out is
  unconstrained — a single hop reaches every member, and tree depth is
  minimal.
- **`Bisection`** as a low-fan-out reference point for benchmarking or when
  the per-hop budget is genuinely the smallest geometrically possible.
- **`BoundedFanout k`** when there is a known per-hop fan-out budget (network
  fan-out cap, per-process outgoing connection limit, etc.). `k` is honored
  at every interior tile, not just at the root.

## Implementation

`src/Tile/Tiling.hs`. Exports `BoundedFanout`, `minimumFanout`, and
`effectiveFanout`; instances `Tiling BoundedFanout`. Helpers `allocateGroups`,
`boundedIntervals`, `frontierTile`, `anchorPrefix`, and `rootPointTile` are
internal. The same `Relation` algebra (`Anchor` / `Sibling`) is shared with
`BlockPartitioning` and `Bisection`; `contractAnchors` requires no changes
for the new tiler.

---

For BoundedFanout k > 4 on 2 x 4 the full local frontier is computed in one shot: dim 0 contributes [E F G H], dim 1 (with dim 0 anchored) contributes [B], [C], [D], and A is the terminal anchor. Then t1 recurses.
```
  ┌──────────────────────────────────────────────────────────┐
  │ t0                                                       │
  │                                                          │
  │  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐                  │
  │  │  A   │  │  B   │  │  C   │  │  D   │                  │
  │  │  t5  │  │  t2  │  │  t3  │  │  t4  │                  │
  │  └──────┘  └──────┘  └──────┘  └──────┘                  │
  │                                                          │
  │  ┌────────────────────────────────────────────────┐      │
  │  │ t1                                             │      │
  │  │  ┌──────┐  ┌──────┐  ┌──────┐  ┌──────┐        │      │
  │  │  │  E   │  │  F   │  │  G   │  │  H   │        │      │
  │  │  │  t9  │  │  t6  │  │  t7  │  │  t8  │        │      │
  │  │  └──────┘  └──────┘  └──────┘  └──────┘        │      │
  │  └────────────────────────────────────────────────┘      │
  └──────────────────────────────────────────────────────────┘

  A (t0)
  ├─ E (t1)
  │  ├─ F (t6)
  │  ├─ G (t7)
  │  └─ H (t8)
  ├─ B (t2)
  ├─ C (t3)
  └─ D (t4)
```
A and E are anchors — structural cover only, not communication children. The send tree root fan-out is 4: B, C, D, and E (root of t1). Then t1 fans out to 3: F, G, H.

With k = 2, allocateGroups gives [1, 1] — one group per active dimension. Dim 0 gets [E F G H], dim 1 (anchored) gets [B C D] as a single slab. Then each recurses with the same cap.
```
  ┌──────────────────────────────────────────────────────────────┐
  │ t0                                                           │
  │                                                              │
  │  ┌──────┐  ┌────────────────────────────────────────────┐    │
  │  │  A   │  │ t2                                         │    │
  │  │  t3  │  │  ┌──────┐  ┌──────┐  ┌──────┐              │    │
  │  └──────┘  │  │  B   │  │  C   │  │  D   │              │    │
  │            │  │  t9  │  │  t7  │  │  t8  │              │    │
  │            │  └──────┘  └──────┘  └──────┘              │    │
  │            └────────────────────────────────────────────┘    │
  │                                                              │
  │  ┌──────────────────────────────────────────────────────┐    │
  │  │ t1                                                   │    │
  │  │  ┌──────┐  ┌────────────────────────┐  ┌──────┐      │    │
  │  │  │  E   │  │ t4                     │  │  H   │      │    │
  │  │  │  t6  │  │  ┌──────┐  ┌──────┐    │  │  t5  │      │    │
  │  │  └──────┘  │  │  F   │  │  G   │    │  └──────┘      │    │
  │  │            │  │  t11 │  │  t10 │    │                │    │
  │  │            │  └──────┘  └──────┘    │                │    │
  │  │            └────────────────────────┘                │    │
  │  └──────────────────────────────────────────────────────┘    │
  └──────────────────────────────────────────────────────────────┘


  A (t0)
  ├─ E (t1)
  │  ├─ F (t4)
  │  │  └─ G (t10)
  │  └─ H (t5)
  └─ B (t2)
     ├─ C (t7)
     └─ D (t8)
```

Root fan-out is 2: E (root of t1) and B (root of t2). Each interior tile fans out at most 2. Compare with k > 4 where t2 was already flat singletons — here t2 still has depth.
