# Viscosity Material Readers (`starter/source/materials/visc/`)

Reads `/VISC` (viscosity) material definitions — bulk and shear viscosity supplementing elastic laws.

## Key Files

| File | Role |
|------|------|
| `hm_read_visc.F` | HM binary reader for `/VISC` keyword |
| `hm_read_visc_prony.F` | HM reader for Prony series viscoelastic model (`/VISC/PRONY`) |
| `hm_read_visc_lprony.F` | HM reader for large-strain Prony series |
| `hm_read_visc_plas.F90` | HM reader for viscoplastic model (`/VISC/PLAS`) |

## Viscosity Models

| Keyword | Model |
|---------|-------|
| `/VISC/KELVIN` | Kelvin-Voigt viscosity: σ_visc = η × dε/dt |
| `/VISC/PRONY` | Prony series (generalised Maxwell): sum of Maxwell elements |
| `/VISC/PLAS` | Viscoplastic (Cowper-Symonds or similar rate-dependent yield) |

Viscosity models are supplemental to the base elastic/plastic law. The material response is the sum of the base elastic stress and the viscous stress.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/README.md` — viscosity laws in engine
