# tile [![haskell ci](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml/badge.svg)](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml) [![docs](https://img.shields.io/badge/docs-github.io-blue)](https://shayne-fletcher.github.io/tile/)

<img src="./images/logo.png"
     alt="Logo"
     style="display: block; margin: 0 auto; width: 32%; height: auto;">

`tile` is a small library for structuring collective communication.

```text
Shape → Tile → Tiling → Schedule → Execution
```

**Shape** defines the dimensions of a rank space.

**Tile** denotes part of a rank space, characterized by a *layout* (how its ranks are organized as N-D — sizes, strides, offset) and a *coverage* (which ranks in the root rank space the tile owns). The global extent is the root tile.

**Tiling** is a strategy for decomposing a tile into child tiles. Different strategies implement the same interface, separating the decomposition algorithm from the tile structure.

**Schedule** is a directed communication plan derived from a tiling — the sequence of edges along which messages flow.

**Execution** interprets a schedule, dispatching messages along the planned edges.

The library is not tied to a particular representation, tiling strategy, or scheduler. It provides a substrate for expressing families of communication patterns as combinations of these orthogonal pieces over a common representation.

Communication structure is constructed as a pure value and interpreted separately. The topology is explicit — it can be inspected, transformed, and reused independently of execution.
