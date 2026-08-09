# LAW88 — Tabulated Elastic Foam (`engine/source/materials/mat/mat088/`)

Direction-dependent tabulated elastic foam with strain-rate effects and
separate loading/unloading curves. Stress is computed by interpolating
uniaxial loading functions g(λ, ε̇) in principal-stretch space; unloading
follows a strain-energy-based trajectory controlled by `iunl_for`.

## Key Files

| File | Role |
|------|------|
| `sigeps88.F90` | Solid stress update: principal-stretch interpolation of loading tables, strain-rate scaling, strain-energy tracking for unloading |
| `sigeps88c.F90` | Shell wrapper (called by `mulawc`) |

## Algorithm Notes

- Stretches and strain rates drive table lookups via `valpvec_v` / `valpvecdp_v` (borrowed from mat033) and `table_mat_vinterp`
- `itens` flag selects tension/compression treatment; `iunl_for` selects unloading formulation
- `uvar(:,1)` stores maximum strain energy (loading envelope); `uvar(:,2)` stores current strain energy

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/tools/README.md` — `table_mat_vinterp` used for table interpolation
- `engine/source/materials/mat/mat033/README.md` — `valpvec_v` / `valpvecdp_v` origin
