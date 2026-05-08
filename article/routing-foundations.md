# Structured Multicast via Affine Tiling

A note on routing foundations for Monarch's cast actor multicast implementation.

**Authors:** Marius Eriksen, Shayne Fletcher, Thomas Wang

**Date:** 2026-05-07

## Introduction

Suppose we have a 2×2 mesh of nodes:
```text
  T = [ a  b
        c  d ]
```
The naive approach requires the client to send one message per node — four messages for a 4-node mesh, and in general one per node. Total delivery work is always linear in the number of nodes; the question is where that work falls.

A better plan exploits fan-out. The client sends only to `a`. Node `a` then forwards to `c`, delegating responsibility for the bottom row, and also to `b`. Node `c` in turn sends to `d`. The client sends one message; the remaining n−1 sends are handled by the mesh itself. The client is no longer the bottleneck — delivery work is distributed across the mesh, and the broadcast completes in O(log n) hop depth.

The same structure scales to larger meshes. Consider a 2×4 mesh:
```text
  T = [ A  B  C  D
        E  F  G  H ]
```

Block partitioning decomposes it recursively — first splitting into top and bottom rows, then splitting each row into individual nodes. The decomposition tree reveals the routing structure:

```text
  T0 [ A  B  C  D
       E  F  G  H ]
  ├─ T2 [ E  F  G  H ]
  │  ├─ T8  [ F ]
  │  ├─ T9  [ G ]
  │  ├─ T10 [ H ]
  │  └─ T7  [ E ]
  └─ T1 [ A  B  C  D ]
     ├─ T4 [ B ]
     ├─ T5 [ C ]
     ├─ T6 [ D ]
     └─ T3 [ A ]
```

Each tile's root forwards to the roots of its children: `A` sends to `E` — delegating the entire bottom row — and to `B`, `C`, and `D`; `E` then sends to `F`, `G`, and `H`. Eight nodes reached in two hops.

What makes this especially powerful is that it composes with subspace selection. A rectangular subgroup of the mesh — selected by fixing a contiguous or strided range along any dimension — is itself an affine subspace, with the same strides as the original and a shifted offset. Block partitioning over that slice gives a self-contained fan-out routing plan scoped exactly to those nodes, with no involvement from the rest of the mesh.

For example, selecting the middle two columns of the 2×4 mesh:
```text
  [ A  B  C  D      →      B  C
    E  F  G  H ]           F  G
```
yields the send tree:
```
  B
  ├─ F
  │  └─ G
  └─ C
```
`B` sends to `F` and `C`; `F` sends to `G`. Two hops, four nodes, the client sends once — and `A`, `D`, `E`, `H` are never touched. The tree topology is determined by the shape of the slice; the actual senders and receivers are additionally fixed by its position in the larger mesh.

All code in the following sections is given in Haskell; readers unfamiliar with the syntax should find it readable as typed pseudocode.

## Affine Rank Space

A mesh of nodes can be addressed in two equivalent ways: by a flat integer index called a rank, or by a multidimensional coordinate called a point. For a 2×4 mesh in row-major order, the correspondence is:
```
    rank:   0  1  2  3
            4  5  6  7

    point:  (0,0)  (0,1)  (0,2)  (0,3)
            (1,0)  (1,1)  (1,2)  (1,3)
```
An *affine rank space* makes this correspondence precise with three fields:

  - **offset** — the rank of the origin, i.e. the node at coordinate (0, 0, …, 0)
  - **sizes** — the extent of the mesh in each dimension
  - **strides** — the rank increment per step in each dimension

For the 2×4 mesh above: `offset = 0`, `sizes = [2, 4]`, `strides = [4, 1]`. The rank of a point `(i, j)` is `offset + i·strides[0] + j·strides[1]`. Moving one step along a row costs 1; moving one step down a column costs 4.

The key operation is `select`, which slices along a dimension. Given a dimension, a begin index, an end index, and a step, it returns a new affine rank space:

  - the offset shifts by `begin · stride[dim]`
  - the size of the selected dimension shrinks accordingly
  - the stride of the selected dimension is multiplied by the step

With `step=1` (a contiguous selection), the strides are unchanged and only the offset and sizes move. Selecting columns 1 and 2 of the 2×4 mesh — `select dim=1 begin=1 end=3 step=1` — gives `offset=1, sizes=[2,2], strides=[4,1]`, covering ranks `{1, 2, 5, 6}`, i.e. `{B, C, F, G}`.

`fixDim` is a special case: it collapses a dimension to a single index, producing a size-1 extent in that dimension. Fixing row 0 of the 2×4 mesh gives `offset=0, sizes=[1,4], strides=[4,1]` — the top row `{A, B, C, D}`.

The crucial property is closure: every result of `select` or `fixDim` is itself an affine rank space. Subgroups defined by rectangular selections — any combination of contiguous or strided ranges along any dimensions — remain in the same family. This is what allows block partitioning to apply uniformly to the full mesh and any of its subspaces.

## Tile

A tile is a view into an affine rank space. It wraps an affine rank space and describes a region of the mesh — a set of nodes identified by their ranks — without owning or copying anything about those nodes themselves.

Two properties of a tile matter most:

**Root**. The root of a tile is the rank at its offset — the node at coordinate (0, 0, …, 0) within the tile. For the full 2×4 mesh the root is rank 0, node `A`. For the middle-column slice — `offset=1, sizes=[2,2], strides=[4,1]` — the root is rank 1, node `B`. The root is the tile's natural representative: the entry point through which a message arrives and from which forwarding begins.

**Membership**. The members of a tile are all the ranks it covers, obtained by varying each dimension's index across its full extent. For the full 2×4 mesh that is `{0,1,2,3,4,5,6,7}`; for the middle-column slice, `{1,2,5,6}`.

A tile carries no information beyond these. It does not know which physical nodes occupy its ranks, whether any of them are reachable, or how messages should flow. That separation is deliberate: a tile is a piece of geometry, and the communication structure built over it comes later.

## Decomposition

Given a tile, block partitioning decomposes it by working through its dimensions one at a time. For each dimension with more than one element, it fixes that dimension to each of its possible indices, producing one child tile per index. It then recurses into the anchor child and continues with the next dimension. The decomposition terminates when every tile is a singleton — a tile with exactly one rank in every dimension.
```text
  ┌──────────────────────────────────┐
  │ t0                               │
  │                                  │
  │  ┌────────────────────────────┐  │
  │  │ t1                         │  │
  │  │  ┌──────┐      ┌──────┐    │  │
  │  │  │  A   │      │  B   │    │  │
  │  │  │  t3  │      │  t4  │    │  │
  │  │  └──────┘      └──────┘    │  │
  │  └────────────────────────────┘  │
  │                                  │
  │  ┌────────────────────────────┐  │
  │  │ t2                         │  │
  │  │  ┌──────┐      ┌──────┐    │  │
  │  │  │  C   │      │  D   │    │  │
  │  │  │  t5  │      │  t6  │    │  │
  │  │  └──────┘      └──────┘    │  │
  │  └────────────────────────────┘  │
  │                                  │
  └──────────────────────────────────┘
```

**Anchor and sibling.** Fixing dimension `dim` to index `i` has a precise effect on the child tile's root:

- At index 0, the offset is unchanged: `root(child) = root(parent)`. This child is the anchor — it inherits the parent's root and carries the decomposition forward through subsequent dimensions.
- At index ≥ 1, the offset shifts by `i · stride[dim]`: `root(child) ≠ root(parent)`. These children are siblings — their roots differ from the parent's, and they will appear as destinations in the send tree.

For a 2×2 tile: fixing row 0 gives anchor `[a, b]` with root `a` unchanged; fixing row 1 gives sibling `[c, d]` with root `c`.

Within `[a, b]`, fixing column 1 gives sibling `[b]`; column 0 gives anchor `[a]`.

**TileNode** and **Relation**. The structural primitive `childNodes` makes the anchor/sibling distinction explicit, labeling each child with the dimension and index that produced it:
```
  Split = { dim : Int, index : Int }

  Relation = Root
           | Anchor   Split
           | Sibling  Split

  TileNode = { tile : Tile, relation : Relation }
```

The `Split` carried by each relation records not just the tree edge but its geometric meaning — which dimension was split and at which index. This is what makes the decomposition tree informative rather than just structural.

**The decomposition tree.** Applying `childNodes` recursively yields the full decomposition tree. For the 2×2 mesh:

    [a b c d]   Root
    ├─ [c d]    Sibling  { dim=0, index=1 }
    │  ├─ [d]   Sibling  { dim=1, index=1 }
    │  └─ [c]   Anchor   { dim=1, index=0 }
    └─ [a b]    Anchor   { dim=0, index=0 }
       ├─ [b]   Sibling  { dim=1, index=1 }
       └─ [a]   Anchor   { dim=1, index=0 }

Every leaf holds exactly one rank. The tree is the complete geometric account of how the tile was decomposed — no communication has been described yet.

## From Decomposition to Schedule

The decomposition tree is a geometric object: it describes how a tile is structured, not how messages flow. The schedule is a communication object: it describes which node sends to which. Moving from one to the other takes three steps, each collapsing one layer of structure.

**Step 1: Decomposition tree.** The tree of `TileNode`s from the previous section — tiles labeled with their structural relation to the parent. For the 2×2 mesh:
```text

      [a b c d]   Root
      ├─ [c d]    Sibling { dim=0, index=1 }
      │  ├─ [d]   Sibling { dim=1, index=1 }
      │  └─ [c]   Anchor  { dim=1, index=0 }
      └─ [a b]    Anchor  { dim=0, index=0 }
         ├─ [b]   Sibling { dim=1, index=1 }
         └─ [a]   Anchor  { dim=1, index=0 }
```

**Step 2: Hop tree.** Read each edge as a root-to-root forwarding step — the parent tile's root to the child tile's root. This projects the decomposition onto potential communication steps:
```text

      a
      ├─ c
      │  ├─ d
      │  └─ c   ← anchor: root([c]) = root([c d])
      └─ a      ← anchor: root([a b]) = root([a b c d])
         ├─ b
         └─ a   ← anchor: root([a]) = root([a b])
```

The anchor edges appear immediately as self-edges. This is not coincidental: fixing index 0 never moves the offset, so an anchor's root is always identical to its parent's root.

**Step 3: Send tree.** Drop the self-edges. Since anchors always produce self-edges and siblings never do, this is equivalent to retaining only the siblings:
```text

      a
      ├─ c
      │  └─ d
      └─ b
```

**Schedule.** Reading off the send tree — taking each parent-to-child edge as a step — gives:
```text
a→c,  a→b,  c→d
```

A step is a sender-receiver pair; a schedule is a list of steps:

```haskell
data Step a = Step { from :: a, to :: a }
type Schedule a = [Step a]
```

In practice, ranks are mapped to actual communicating members — the processes that will send and receive. For the 2×2 mesh this gives `[Step a c, Step a b, Step c d]`.

**nextHops.** The function `nextHops` implements the send tree traversal: given a `TileNode`, it descends through anchors and collects siblings. `children` follows directly:
```haskell
  nextHops :: BlockPartitioning -> TileNode -> [TileNode]

  children :: BlockPartitioning -> Tile -> [Tile]
  children tiling = map tile . nextHops tiling . rootNode
    where rootNode t = TileNode t Root
```

## Subspace Routing

Block partitioning requires nothing special of the tile it operates on. Any affine subspace — produced by `select` or `fixDim` from a larger space — is a tile, and the same algorithm applies without modification. The resulting schedule is scoped exactly to the members of that subspace.

Taking the middle two columns of the 2×4 mesh:
```text
    [ A  B  C  D      →      B  C
      E  F  G  H ]           F  G
```

This slice has `offset=1, sizes=[2,2], strides=[4,1]` — the same strides as the full mesh, at a shifted offset. Block partitioning yields:
```text
    [B C F G]   Root
    ├─ [F G]    Sibling { dim=0, index=1 }
    │  ├─ [G]   Sibling { dim=1, index=1 }
    │  └─ [F]   Anchor  { dim=1, index=0 }
    └─ [B C]    Anchor  { dim=0, index=0 }
       ├─ [C]   Sibling { dim=1, index=1 }
       └─ [B]   Anchor  { dim=1, index=0 }
```
and the send tree:
```text
    B
    ├─ F
    │  └─ G
    └─ C
```
Three edges, two hops, four nodes. No node outside the slice appears anywhere in the decomposition.

**Fault-free correctness.** For a subspace of n nodes:

- **n−1 edges**. The send tree is a spanning tree of the n members. A spanning tree has exactly n−1 edges, so the schedule contains exactly n−1 steps — one message received per node, none redundant.
- **O(log n) hops.** Block partitioning contributes at most one level to the send tree per dimension — each dimension produces one sibling split and then recurses into the anchor. The depth therefore equals the number of non-trivial dimensions d. Since each non-trivial dimension has size at least 2, the tile contains at least 2ᵈ nodes; but the tile has exactly n nodes, so 2ᵈ ≤ n, giving d ≤ log₂ n. The broadcast completes in at most log₂ n hops. Note that this bounds hop *depth*, not per-node fan-out. In a 1×n tile the root sends to n−1 children in a single hop; depth is 1 but fan-out is n−1. Fan-out is shape-dependent.
- **No outside involvement.** Every rank in the schedule belongs to the slice. Nodes outside it neither send nor receive.

**Shape determines routing.** The tree topology depends only on sizes — the branching structure is the same for any two tiles with the same extents. The concrete senders and receivers are additionally determined by strides and offset: strides govern which ranks fall in each tile, offset fixes the ingress point. The middle-column slice and a standalone 2×2 mesh are topologically isomorphic; their actual members differ.

Carve out any rectangular subgroup of the mesh and block partitioning delivers a self-contained broadcast plan for exactly that group: a spanning tree with n−1 edges, O(log n) hop depth, and no involvement from nodes outside the selected subspace.

## Representative Selection

Geometry determines the tile; tiling determines the tree; representative policy determines who speaks for each tile.

In the fault-free setting, the representative of a tile is simply its root — the member at the tile's offset, the natural entry point for schedule derivation by taking roots. Two practical pressures require something more flexible: nodes fail, and the natural root may not be the best sender. Both are instances of the same question: *which member speaks for a tile?*

**Occlusion.** When nodes fail, the failed members are marked via a predicate:
```haskell
  newtype Occlusion a = Occlusion { isOccluded :: a -> Bool }
```

Occlusion does not change the tile or its decomposition. It changes only which live member represents each tile: if the natural root is live, it remains the representative; otherwise another live member in the tile is chosen.
```haskell
  representative :: Eq a => Occlusion a -> [a] -> Tile -> Maybe a
  representative occ members tile =
    find (not . isOccluded occ) [ members !! r | r <- tileRanks tile ]
```
If no live member exists, `representative` returns `Nothing` and the subtree is pruned entirely — the practical case being a rack or zone failure that takes out a whole subgroup.

**`stepForOccluded`.** Schedule steps are built from representatives rather than roots:
```haskell
  stepForOccluded :: Eq a => Occlusion a -> [a] -> Tile -> Tile -> Maybe (Step a)
  stepForOccluded occ members parent child =
    case (representative occ members parent, representative occ members child) of
      (Just p, Just c) | p /= c -> Just (Step { from = p, to = c })
      _                          -> Nothing
```

**`RoutedSchedule`.** Under occlusion the ingress may no longer be the tile's natural root, so the result names it explicitly:
```haskell
  data RoutedSchedule a = RoutedSchedule
    { ingress     :: a
    , routedSteps :: Schedule a
    }
```

**Example: 2×2 with `c` failed.** In the fault-free setting the schedule is `[a→c, a→b, c→d]`. With `c` occluded:

  - representative of tile `[c d]` returns `d`
  - `stepForOccluded` produces `a→d` in place of `a→c`
  - The step `c→d` drops: `representative [c d] = d` = `representative [d]`, so `p /= c` fails
  - Result: `ingress = a`, `routedSteps = [a→d, a→b]`

The subtree `[c d]` survives, rerooted at `d`. No change to the geometry.

**Rerooting and pruning in a 2×4 mesh.** The same mechanics scale up. Consider a single node failure — E, the natural root of the bottom row, goes down. The bottom row reroots at F:
```text
    fault-free          E failed

    A                   A
    ├─ E                ├─ F
    │  ├─ F             │  ├─ G
    │  ├─ G             │  └─ H
    │  └─ H             ├─ B
    ├─ B                ├─ C
    ├─ C                └─ D
    └─ D
```

When the failure is wider — the entire bottom row gone, a rack failure — `representative` finds no live member in `[E F G H]` and the subtree is pruned:
```text
    A
    ├─ B
    ├─ C
    └─ D
```
The top row continues to receive. The schedule contracts to fit what is reachable.

The same mechanism handles jagged regions: embed the participant set in its smallest affine bounding tile, mark the gaps as occluded, and representative selection does the rest — sparse participation and node failure are the same problem at this layer.

For example, a lower-right triangular region in a 3×3 mesh:
```text
  . . C
  . E F
  G H I
```
The affine envelope is the full 3×3; A, B, and D are occluded. The geometric root of the envelope is A — but A is unavailable, so `representative` finds C instead. The send tree:
```text
C
├─ E
│  └─ F
└─ G
   ├─ H
   └─ I
```
The ingress shifts to the first live member; the tree covers exactly the participating nodes.

**Load balancing.** Representative selection is a policy seam. Occlusion picks the first live member; load balancing picks the least loaded one. The tile tree and schedule derivation are identical in both cases — only `representative` changes.
