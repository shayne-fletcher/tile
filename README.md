# tile [![haskell ci](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml/badge.svg)](https://github.com/shayne-fletcher/tile/actions/workflows/ci.yml) [![docs](https://img.shields.io/badge/docs-github.io-blue)](https://shayne-fletcher.github.io/tile/)

<img src="./images/logo.png"
     alt="Logo"
     style="display: block; margin: 0 auto; width: 32%; height: auto;">

`tile` is a small library for structuring collective communication.

```text
Shape → Tile → Tiling → Schedule → Execution
```

Shape defines a rank space.

Tile is a region within that space.

Tiling defines a decomposition of regions.

Schedule is a directed communication plan.

Execution interprets that plan.

The library is not tied to a particular algorithm. It provides a substrate for expressing families of communication patterns as combinations of tilings and schedulers over a common representation.

Communication structure is constructed as a pure object and interpreted separately.

The topology is explicit. It can be inspected, transformed, and reused independently of execution.
