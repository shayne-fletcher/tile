# tile

A small Haskell library exploring multicast routing via recursive block partitioning.

Pipeline:

Shape → Tile → Tiling → Schedule → Execution

Includes:
- Block-partitioning tiling
- DFS scheduler
- Chan-based concurrent execution

Inspired by distributed systems routing (Monarch, MPI, NCCL).
