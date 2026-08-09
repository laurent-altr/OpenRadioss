# Starter LAW85 — Elastic-plastic with temperature-dependent properties (`starter/source/materials/mat/mat085/`)

Reads `/MAT/LAW85` keyword parameters from the Radioss deck and stores
them in the `PM` / `IPM` arrays passed to the engine material routine.

## Key Files

| File | Role |
|------|------|
| `lecm85_void.F` | `LECM85_VOID` — starter helper routine |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/materials/mat/mat085/README.md` — corresponding engine law
