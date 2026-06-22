# LAW109 — Tabulated Visco-Elastic (`engine/source/materials/mat/mat109/`)

Visco-elastic material with tabulated frequency-domain master curves
(storage modulus G', loss modulus G"); converted to Prony series at run time.

## Key Files

| File | Role |
|------|------|
| `sigeps109.F` | Main stress update: Prony integration from fitted master curves |
| `sigeps109c.F` | Shell variant |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/materials/visc/README.md` — shared Prony utilities
