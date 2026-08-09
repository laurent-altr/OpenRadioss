# SPMD Tools (`starter/source/spmd/tools/`)

Utility routines for MPI domain-decomposition post-processing.

## Key Files

| File | Role |
|------|------|
| `apply_permutation.F90` | Reorder arrays according to a permutation vector (reorder nodes/elements in rank-local order after METIS) |
| `init_mid_pid_array.F` | Initialise material ID and property ID lookup arrays (mid/pid) in rank-local numbering |

## Permutation After METIS

After METIS returns the partition, element and node arrays must be reordered so that local entities appear contiguously in memory. `apply_permutation.F90` applies an in-place permutation to arrays like `X` (node coordinates), `IRECT` (element connectivity), and `PM`/`IPM` (material parameters).

This reordering is essential for cache-efficient access in the engine — the inner time-integration loops iterate over elements and nodes in sequential array order, so having local elements/nodes contiguous maximises cache hits.

## Related Documentation

- `starter/source/spmd/README.md` — parent SPMD directory
- `starter/source/spmd/domain_decomposition/README.md` — METIS partition
- `starter/source/spmd/node/README.md` — ghost node lists
