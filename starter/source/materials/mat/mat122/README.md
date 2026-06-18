# Starter LAW122 — Elastic-plastic with tabulated hardening (Hershey yield) Reader (`starter/source/materials/mat/mat122/`)

Reads `/MAT/LAW122` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat122.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat122/README.md` — corresponding engine law
