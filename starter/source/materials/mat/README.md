# Starter Material Law Input (`starter/source/materials/mat/`)

Per-law subdirectories containing input readers for each material law (`/MAT/LAWNN`). One subdirectory per law number (`mat000`, `mat001`–`mat190`, `matgas`, `matuser`).

## Top-Level Files

| File | Role |
|------|------|
| `hm_read_mat.F90` | Master material reader: dispatch to per-law subdirectory |
| `ini_mat_elem.F` | Initialise material-element association arrays |
| `init_mat_keyword.F` | Register all material keywords in the keyword dispatch table |
| `check_mat_elem_prop_compatibility.F` | Validate material / element / property combination |

## Subdirectory Naming

Each `matNNN/` contains `hm_read_matNNN.F` (the card reader for `/MAT/LAWNN`) and any auxiliary readers for sub-options. Special directories:

| Directory | Contents |
|-----------|---------|
| `mat000/` | Null/void material |
| `mat131/` | Complex LAW131 (multi-sub-model): subdirectories for elasticity, hardening, rate dependency, thermal softening, etc. |
| `matgas/` | Ideal gas material for ALE/Euler |
| `matuser/` | User material (`/MAT/LAWNN` where NN is user-defined) |

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/mat/README.md` — engine-side constitutive routines
