# Starter LAW106 — Elastic-plastic with tabulated strain-rate (Huh-Kang) Reader (`starter/source/materials/mat/mat106/`)

Reads `/MAT/LAW106` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat106.F90` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat106/README.md` — corresponding engine law
