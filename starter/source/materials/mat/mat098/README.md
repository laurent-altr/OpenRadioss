# Starter LAW98 — Elastic-plastic with tabulated hardening and Hosford yield (`starter/source/materials/mat/mat098/`)

Reads `/MAT/LAW98` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `lossfun_98.F` | `LOSSFUN_98` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat098/README.md` — corresponding engine law
