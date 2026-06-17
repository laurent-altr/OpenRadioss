# SPH Force Computation (`engine/source/elements/sph/`)

Computes Smoothed Particle Hydrodynamics (SPH) particle forces each time step.

## Key Files

| File | Role |
|------|------|
| `soltosph.F` | Convert brick elements to SPH particles mid-run (`/SOL2SPH` trigger) |
| `soltosph_hour.F` | Convert with hourglass mode transfer |
| `soltosph_on1.F` / `soltosph_on2.F` | Activate converted SPH particles in stage 1/2 |
| `soltospha.F` | Alternative SOL2SPH conversion path |
| `spdefo3.F` | Compute SPH particle deformation gradient (strain rate) |
| `spdens.F` | Compute SPH particle density via kernel sum: ρ_i = Σ_j m_j W(r_ij, h) |
| `spbilan.F` | SPH energy balance |
| `spclasv.F` | Classify SPH particles (bulk / surface / void) |
| `spcompl.F` | Complement SPH force (add tensile instability correction) |
| `spback3.F` | Background pressure for tensile SPH stability |
| `spbuc3.F` | SPH buckling correction |
| `spadah.F` | Adaptive smoothing length `h` update |
| `spadasm.F` | Adaptive smoothing with mass conservation |
| `spechan.F` | SPH-to-element force exchange (SPH coupled to FE mesh) |

## SPH Force Computation

Each time step:
1. **Neighbourhood search**: find all particles within `κh` of each particle (using spatial hash)
2. **Density** (`spdens.F`): compute ρ_i = Σ_j m_j W(r_ij, h) via kernel summation
3. **Pressure**: p = EOS(ρ, e) — equation of state
4. **Force**: f_i = -Σ_j m_j (p_i/ρ_i² + p_j/ρ_j²) ∇W(r_ij, h)
5. **Adaptive h** (`spadah.F`): update smoothing length to maintain ~50 neighbours per particle

## Related Documentation

- `starter/source/elements/sph/README.md` — SPH initialisation
- `engine/source/elements/README.md` — parent elements overview
