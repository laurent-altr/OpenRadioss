# LAW26 — Honeycomb (`engine/source/materials/mat/mat026/`)

Crushable honeycomb material: fully decoupled directional normal and shear
responses, each with tabulated compaction curves. Used for energy absorbers.

## Key Files

| File | Role |
|------|------|
| `m26law.F` | Main stress update: independent X/Y/Z and shear integrations |
| `m26th.F` | Thermal coupling |
| `mindex.F` | Index-search helper for tabulated lookup |
| `sesa10.F` / `sesa20.F` / `sesa30.F` | Sub-element surface-area computations |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
