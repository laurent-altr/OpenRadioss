# Hashin Failure Criterion (`engine/source/materials/fail/hashin/`)

Implements the Hashin (1980) composite failure criteria: separate quadratic interaction criteria for fiber tension/compression and matrix tension/compression.

## Key Files

| File | Role |
|------|------|
| `fail_hashin_c.F90` | Hashin failure check for solid elements |
| `fail_hashin_s.F90` | Hashin failure check for shell elements |

## Criterion

Four independent failure modes:

| Mode | Criterion |
|------|-----------|
| Fiber tension | `(σ_11/X_T)² + (τ_12/S_12)² ≥ 1` |
| Fiber compression | `(σ_11/X_C)² ≥ 1` |
| Matrix tension | `(σ_22/Y_T)² + (τ_12/S_12)² ≥ 1` |
| Matrix compression | `(σ_22/2S_23)² + [(Y_C/2S_23)²−1](σ_22/Y_C) + (τ_12/S_12)² ≥ 1` |

Each mode triggers independently. Typically used with unidirectional and woven composite material laws (LAW025, LAW058).

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/puck/README.md` — Puck (more physically refined)
- `engine/source/materials/fail/changchang/README.md` — Chang-Chang variant
