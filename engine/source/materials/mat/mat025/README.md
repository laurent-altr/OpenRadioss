# LAW25 — Composite with Delamination (Tsai-Wu + Prony) (`engine/source/materials/mat/mat025/`)

Orthotropic elastic material with Tsai-Wu failure criterion, crack-survival
surface, delamination modelling, and optional Prony visco-elastic response.

## Key Files

| File | Role |
|------|------|
| `m25law.F` | Main stress update: orthotropic elasticity + Tsai-Wu + delam |
| `m25cplrc.F` | Shell/composite layer coupler |
| `m25crak.F` | Crack-survival surface tracking |
| `m25delam.F` | Delamination onset and growth |
| `prony25c.F` | Prony-series visco-elastic contribution |
| `sigeps25c.F` / `sigeps25cp.F` | Shell stress-update wrappers |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
