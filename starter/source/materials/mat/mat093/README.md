# Starter LAW93 — Elastic-plastic with tabulated hardening and anisotropy (BBC2003) Reader (`starter/source/materials/mat/mat093/`)

Reads `/MAT/LAW93` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `hm_read_mat93.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat093/README.md` — corresponding engine law
