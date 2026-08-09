# Turbulence Models (`engine/source/ale/turbulence/`)

Implements turbulence models for ALE/Euler fluid simulations.

## Key Files

| File | Role |
|------|------|
| `aeturb.F` | Main turbulence driver: apply turbulence model to ALE elements |
| `akturb.F` | k-ε turbulence model: compute turbulent kinetic energy `k` and dissipation rate `ε` |
| `atur17.F` | TYPE17 turbulence model (Smagorinsky LES sub-grid scale) |
| `aturbn.F` | Turbulent viscosity: compute `μ_t = C_μ ρ k²/ε` |
| `a17yl2.F` | 2D yield criterion for turbulent ALE |
| `a17yl3.F` | 3D yield criterion for turbulent ALE |

## k-ε Model

The k-ε model adds two transport equations to the ALE system:
- Turbulent kinetic energy: `∂(ρk)/∂t + ∇·(ρku) = P_k - ρε + ∇·(μ_t/σ_k ∇k)`
- Dissipation rate: `∂(ρε)/∂t + ∇·(ρεu) = C_1ε P_k ε/k - C_2ε ρε²/k`

where `P_k` is production and `μ_t = C_μ ρ k²/ε` is the eddy viscosity.

Used for high-Reynolds-number ALE fluid flow (blast wave propagation, air blast in enclosures).

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
