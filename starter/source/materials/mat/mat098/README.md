# Starter LAW98 — Elastic-plastic with tabulated hardening and Hosford yield Reader (`starter/source/materials/mat/mat098/`)

Reads `/MAT/LAW98` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `lossfun_98.F` | Starter input reader / initialiser |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat098/README.md` — corresponding engine law
