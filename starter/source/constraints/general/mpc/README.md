# Multi-Point Constraints (`starter/source/constraints/general/mpc/`)

Reads /MPC cards defining linear multi-point constraints between DOFs.

## Key Files

| File | Role |
|------|------|
| `hm_read_mpc.F` | Parse /MPC: master/slave DOF pairs, coefficient matrix |

## Description

`/MPC` enforces linear relationships between DOFs: `Σ aᵢ uᵢ = b`. Typical uses include tying non-conforming meshes, coupling beam nodes to shell surfaces, and implementing rigid link constraints. The starter writes the coefficient matrix to the restart file for enforcement via Lagrange multipliers or penalty in the engine.

## Related Documentation

- `starter/source/constraints/general/README.md` — parent directory
- `engine/source/tools/lagmul/README.md` — Lagrange multiplier solver
