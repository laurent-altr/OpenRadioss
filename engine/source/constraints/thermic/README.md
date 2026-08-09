# Thermal Constraints (`engine/source/constraints/thermic/`)

Applies thermal boundary conditions for thermomechanically coupled simulations.

## Key Files

| File | Role |
|------|------|
| `fixtemp.F` | Apply fixed temperature BC: set nodal temperature to prescribed value |
| `fixflux.F` | Apply fixed heat flux BC: add prescribed flux to nodal energy |
| `convec.F` | Apply convective heat transfer BC: `q = h × (T_surface - T_ambient)` |
| `convecoff.F` | Deactivate convection BC when sensor triggers |
| `radiation.F` | Apply radiative heat transfer: `q = ε × σ × (T⁴ - T_amb⁴)` (Stefan-Boltzmann) |
| `radiatoff.F` | Deactivate radiation BC |
| `tempur.F` | Update temperature field after thermal constraint application |
| `thermbilan.F` | Thermal energy balance: compute heat flux residual |
| `frethermal.F` | Read engine-side thermal BC keyword (`/EBCS/THERMAL`) |
| `fxfluxrrest.F` | Restore flux BC from restart file |
| `fxfluxwrest.F` | Write flux BC to restart file |

## Thermal BC Types

| BC | Formula |
|----|---------|
| Fixed temperature | T = T_prescribed |
| Fixed flux | q = q_prescribed |
| Convection | q = h(T − T∞) |
| Radiation | q = εσ(T⁴ − T∞⁴) |
| Adiabatic | q = 0 (default, no BC) |

These are applied to element faces (surface BCs) or nodes (point BCs). Combined thermo-mechanical coupling: mechanical dissipation heats the material (Taylor-Quinney), and temperature feeds back into yield stress.

## Related Documentation

- `engine/source/constraints/README.md` — parent constraints directory
- `engine/source/boundary_conditions/README.md` — EBCS thermal and other surface BCs
