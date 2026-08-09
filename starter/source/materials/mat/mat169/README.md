# Starter LAW169 — Elastic-plastic with tabulated hardening (reserved / future) (`starter/source/materials/mat/mat169/`)

Reads `/MAT/LAW169` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat169.F90` | `HM_READ_MAT169_ARUP` — reads `/MAT/LAW169` arup variant |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat169/README.md` — corresponding engine law
