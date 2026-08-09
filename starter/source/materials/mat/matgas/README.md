# Matgas — Ideal Gas Material Reader (`starter/source/materials/mat/matgas/`)

Starter reader for the `/MAT/GAS` (ideal gas) material law used in
control-volume airbag and ALE gas models.

## Key Files

| File | Role |
|------|------|
| `hm_read_matgas.F` | `HM_READ_MATGAS` — reads `/MAT/GAS` keyword parameters (γ, C_v, initial pressure) into `PM`/`IPM` arrays |

## Related Documentation

- `starter/source/materials/mat/README.md` — parent material reader directory
- `engine/source/airbag/README.md` — control-volume airbag consumer of gas properties
