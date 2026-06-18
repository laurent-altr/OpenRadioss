# Starter LAW126 — Elastic-plastic with Barlat Yld2004 and tabulated hardening Reader (`starter/source/materials/mat/mat126/`)

Reads `/MAT/LAW126` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat126.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat126/README.md` — corresponding engine law
