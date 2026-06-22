# Viscosity Laws (`engine/source/materials/visc/`)

Implements viscoelastic constitutive models: Prony series (linear viscoelasticity) and viscoplastic flow rules.

## Key Files

| File | Role |
|------|------|
| `viscmain.F` | Main viscoelasticity dispatcher |
| `visc_prony.F` | Prony series viscoelastic update: exponential relaxation for each term |
| `visc_prony_lstrain.F` | Prony series at large strain (finite viscoelasticity) |
| `visc_plas.F90` | Viscoplastic flow: Perzyna/Duvaut-Lions type rate-dependent plasticity |
| `prony_modelc.F` | Prony model for compressible bulk response |

## Algorithm

The Prony series represents the relaxation modulus as:
```
E(t) = E∞ + Σ Eₙ exp(−t/τₙ)
```

`visc_prony.F` updates the internal variables (hidden stresses) for each Prony term using the exact exponential integration scheme (unconditionally stable for any step size). `visc_plas.F90` applies a viscoplastic corrector to the trial stress using Perzyna's overstress formulation `ε̇_p = ⟨(f/σ_0)^n⟩`.

## Related Documentation

- `engine/source/materials/README.md` — parent materials directory
- `engine/source/materials/mat_share/README.md` — material dispatch
