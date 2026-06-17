# Thermal Material Readers (`starter/source/materials/therm/`)

Reads thermal material parameters for thermomechanically coupled simulations.

## Key Files

| File | Role |
|------|------|
| `hm_read_therm.F` | HM binary reader for `/MAT/THERM` — thermal conductivity, specific heat, density |
| `hm_read_therm_stress.F90` | HM reader for thermal stress coupling parameters (thermal expansion coefficient) |
| `initemp_shell.F90` | Initialise temperature field on shell elements (initial temperature distribution) |

## Thermal Parameters Read

| Parameter | Symbol | Usage |
|-----------|--------|-------|
| Thermal conductivity | λ | Heat conduction equation |
| Specific heat | Cp | Thermal mass |
| Thermal expansion coefficient | α | Thermal strain ε_th = α × ΔT |
| Taylor-Quinney coefficient | β | Fraction of plastic work converted to heat |

These parameters supplement the mechanical material law and are stored in the same `PM` array.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `engine/source/materials/README.md` — thermal-mechanical coupling in engine
