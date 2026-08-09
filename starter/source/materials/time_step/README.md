# Material Time Step (`starter/source/materials/time_step/`)

Computes element time-step estimates based on material wave speed during starter initialisation, providing the initial CFL time step.

## Key Files

| File | Role |
|------|------|
| `dt1law.F` | Time step for standard elastic laws (LAW1, LAW2): `dt = L / sqrt(E/ρ)` |
| `dt10law.F` | Time step for LAW10 (composite shell) |
| `dt11law.F` | Time step for LAW11 |
| `dt14law.F` | Time step for LAW14 |
| `dt15law.F` | Time step for LAW15 |
| `dt16law.F` | Time step for LAW16 |
| `dt18law.F` | Time step for LAW18 |
| `dt21law.F` | Time step for LAW21 |
| `dt105law.F` | Time step for LAW105 |
| `dt138aw.F` | Time step for LAW138 |

## Purpose

Each law may have a different wave speed formula depending on its material model. For example:
- Elastic: `c = sqrt(E/ρ)` (longitudinal wave speed)
- Rubber (LAW42): `c = sqrt(bulk_modulus/ρ)`
- Composite (LAW25): `c = sqrt(E11/ρ)` (fastest wave direction)

The starter computes the initial time step per element using the appropriate law-specific formula. The engine recomputes these estimates each step as the material state evolves.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/time_step/README.md` — engine time step computation
