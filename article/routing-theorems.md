# Cast Routing Correctness via Affine Tiling

A correctness contract for Monarch cast actor routing.

This note states the correctness target for Monarch cast actor routing: given an affine target tile and an availability predicate, a cast should deliver exactly once to every live actor in the target tile, never deliver outside the target tile, and never route through unavailable actors.

The companion note, `routing-foundations.md`, explains how the algebra is built. This note states the delivery contract that an implementation can be checked against.

The Haskell model, [`tile`](https://github.com/shayne-fletcher/tile.git), is small enough to support these claims with QuickCheck properties.

## Notation

Let `T` be an affine target tile.

- `R(T)` is the set of ranks in the target tile
- `root(T)` is the offset rank of the tile

Let `E` be a schedule, i.e. a finite list of directed send steps.

- `senders(E)` is the set of sources in `E`
- `receivers(E)` is the set of destinations in `E`

Let `sched(T)` be the fault-free schedule produced by block partitioning.

Let `live ⊆ R(T)` be the set of available members in the target tile. Equivalently, `live` is the complement of an occlusion predicate restricted to `R(T)`.

Let `occSched(T, live)` be the routed schedule produced under that availability set.

## Geometric Laws

The low-level affine facts are prerequisites:

- ranks and points round-trip
- rank enumeration is exact
- affine slicing produces affine subspaces
- affine slicing stays inside its parent space
- decomposition children stay inside their parent tile

These are properties of the representation, not the main routing result. They are the geometry that makes the delivery theorems meaningful.

## Theorem T1: Fault-Free Cast Coverage

Let `T` be a non-empty affine target tile, and let:

```text
n = |R(T)|
sched(T) = E
```

Then `E` is a spanning send tree over `R(T)` rooted at `root(T)`:

```text
|E| = n - 1
receivers(E) = R(T) - {root(T)}
each receiver appears exactly once
senders(E) ⊆ R(T)
receivers(E) ⊆ R(T)
```

Equivalently: every non-root target receives exactly once, the root does not receive, and no actor outside the target tile participates.

This theorem states delivery behavior, not schedule order. DFS and BFS may satisfy the same law with different edge orderings.

## Theorem T2: Occluded Cast Coverage

If `live = ∅`, then:

```text
occSched(T, live) = Nothing
```

If `live ≠ ∅`, then:

```text
occSched(T, live) = Just (ingress, E)
```

where:

```text
ingress ∈ live
receivers(E) = live - {ingress}
each receiver appears exactly once
senders(E) ⊆ live
receivers(E) ⊆ live
```

Equivalently: the ingress is live, every other live target receives exactly once, and unavailable actors neither send nor receive.

The affine geometry is unchanged. Occlusion only changes representative selection and pruning.

## Corollary C1: Jagged Regions

Let `J` be a jagged participant set inside an affine envelope `T`. Define:

```text
live = J
occluded = R(T) - J
```

Then routing the envelope under `live` delivers exactly to the jagged region:

```text
receivers(E) ∪ {ingress} = J
```

Sparse participation and node failure are the same problem at this layer.

## Corollary C2: Monarch Cast Correctness Target

For a Monarch cast implementation, the core correctness target is:

```text
Given an affine target tile and an availability predicate,
cast delivers exactly once to every live actor in the target tile,
never delivers to an actor outside the target tile,
never delivers to an unavailable actor,
and every forwarding actor is live under the representative policy.
```

Internal reshape is allowed to change the routing tree, but not the target set. A reshaped cast should deliver to the same live members of the affine target tile as the unreshaped model. This preservation law is the next piece to model explicitly.

## Supporting Properties

The Haskell model supports these claims with QuickCheck properties in [`test/Main.hs`](https://github.com/shayne-fletcher/tile/blob/main/test/Main.hs):

```text
affine rank/point roundtrip
ranks enumerate the affine space exactly once
affine slicing is closed and included in its parent
structural and communication children are included in their parent
fault-free schedules form a spanning send tree
occluded schedules deliver exactly to live members
```

The first four properties support the geometric assumptions. The last two correspond directly to T1 and T2.
