# tile [![haskell ci](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml/badge.svg)](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml) [![docs](https://img.shields.io/badge/docs-github.io-blue)](https://shayne-fletcher.github.io/tile/)

<img src="./images/logo.png"
     alt="Logo"
     style="display: block; margin: 0 auto; width: 32%; height: auto;">

`tile` is a small library for structuring collective communication over affine views of a rank space.

```text
Shape → AffineRankSpace → Tile → Tiling/Tree → Schedule → Execution
```

**Shape** defines the dimensions of a root rank space.

**AffineRankSpace** is the core representation of an N-D rank space: `offset`, `sizes`, and `strides`. `select` and `fixDim` produce affine subspaces, so slices of a mesh stay in the same representation.

**Tile** wraps an affine rank space. Its root is the tile's offset; its members are the ranks covered by that affine view.

**Tiling** is a strategy for decomposing a tile into child tiles. `BlockPartitioning` exposes both the structural decomposition (`childNodes`, with `Anchor`/`Sibling` relations) and the communication projection (`nextHops`).

**Tree** materializes those views as decomposition, hop, and send trees.

**Schedule** is a directed communication plan derived from a tiling by taking roots along the communication tree.

**Execution** interprets a schedule, dispatching messages along the planned edges.

The library currently centers affine slicing and block partitioning. A full mesh and any row, column, or strided slice are all expressed as tiles, and the same scheduling machinery applies to each.

Communication structure is constructed as a pure value and interpreted separately. The topology is explicit — it can be inspected, transformed, and reused independently of execution.
