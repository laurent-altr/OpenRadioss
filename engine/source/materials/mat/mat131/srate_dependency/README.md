# LAW131 — Strain-Rate Dependency Modules (`engine/source/materials/mat/mat131/srate_dependency/`)

Modular strain-rate scaling functions for the LAW131 yield stress.

## Key Files

| File | Role |
|------|------|
| `srate_dependency_johnsoncook.F90` | Johnson-Cook logarithmic rate factor: (1 + C ln ε̇*) |
| `srate_dependency_cowpersymonds.F90` | Cowper-Symonds power-law rate factor |
| `srate_dependency_nonlinear.F90` | Nonlinear rate function (physically-motivated) |
| `srate_dependency_tabulated.F90` | Tabulated σ_y(ε̇) multiplier |

## Related Documentation

- `engine/source/materials/mat/mat131/README.md` — parent directory
