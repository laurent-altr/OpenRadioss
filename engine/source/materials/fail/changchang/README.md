# Chang-Chang Failure Criterion (`engine/source/materials/fail/changchang/`)

Implements the Chang-Chang (1987) progressive failure criterion for fiber-reinforced composites with degraded stiffness after failure.

## Key Files

| File | Role |
|------|------|
| `fail_changchang_c.F90` | Chang-Chang for solid elements |
| `fail_changchang_s.F90` | Chang-Chang for shell elements |

## Criterion

Four failure modes with stiffness degradation:

| Mode | Condition | Degradation |
|------|-----------|-------------|
| Fiber tension | `(σ_11/X_T)² + (τ_12/S_12)² ≥ 1` | `E_11, E_22, G_12 → 0` |
| Fiber compression | `σ_11 < 0` and `(σ_11/X_C)² ≥ 1` | `E_11 → 0` |
| Matrix tension | `σ_22 ≥ 0` and matrix criterion ≥ 1 | `E_22, G_12 → 0` |
| Matrix compression | `σ_22 < 0` and matrix criterion ≥ 1 | `E_22, G_12 → 0` |

Chang-Chang differs from Hashin in using a modified shear nonlinearity term `α τ_12³ / G_12` in the fiber mode. Often used with shell properties for crash simulation of CFRP panels.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/hashin/README.md` — Hashin (no degradation model)
