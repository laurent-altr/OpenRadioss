# LAW51 — Explosive / JWL + Ignition (`engine/source/materials/mat/mat051/`)

High-explosive material using JWL equation of state with ignition-and-growth
detonation model, Drucker-Prager deviatoric plasticity, and heat generation.

## Key Files

| File | Role |
|------|------|
| `jwl51.F` | Jones-Wilkins-Lee (JWL) EOS pressure computation |
| `dprag51.F` | Drucker-Prager deviatoric stress update |
| `heat51.F` | Exothermic heat release from ignition kinetics |
| `compute_bfrac.F` | Burn fraction α evolution (pressure-based ignition) |
| `m51vois2.F` / `m51vois3.F` | Neighbour-cell communication (2D / 3D) |
| `write_buf_law51.F` | Animation output buffer writer |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/ale/README.md` — ALE detonation simulations
