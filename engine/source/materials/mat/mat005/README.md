# LAW5 — Elastic Hydrodynamic with JWL EOS (`engine/source/materials/mat/mat005/`)

Elastic-hydrodynamic material using a Jones-Wilkins-Lee (JWL) equation
of state. Primarily used for solid explosive products and shock physics.

## Key Files

| File | Role |
|------|------|
| `m5law.F` | Deviatoric stress integration; calls JWL EOS for pressure |
| `mjwl.F` | JWL EOS: p = A(1−ω/R₁V)e^{−R₁V} + B(1−ω/R₂V)e^{−R₂V} + ωE/V |

## Related Documentation

- `engine/source/materials/mat/README.md` — parent directory
- `engine/source/ale/README.md` — ALE explosives simulations
