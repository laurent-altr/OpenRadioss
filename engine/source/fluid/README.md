# Fluid Subsystem

This subsystem implements a Navier-Stokes incompressible fluid solver for Eulerian or ALE fluid domains within OpenRadioss. It is distinct from the ALE/Euler compressible solver in `ale/` — this subsystem targets incompressible viscous flows.

## Scope

The fluid subsystem is used for:
- Fluid-structure interaction (FSI) where the fluid is incompressible
- Viscous flow in coupled structural problems
- Acoustic boundary element method (BEM) for noise/acoustic FSI

Activated by `/FLUID` or related keywords.

## Key Files

| File | Role |
|------|------|
| `flow0.F` | Incompressible flow initialisation |
| `flow1.F` | Incompressible flow time step computation |
| `incpflow.F` | Incompressible Navier-Stokes solver driver |
| `fluxsw.F` | Flux switching between fluid and structural domains |
| `lecflsw.F` | Read fluid switch parameters |
| `nintrn.F` | Fluid-structure interface normal computation |
| `daaacc.F` | DAA (Doubly Asymptotic Approximation) — acoustic acceleration |
| `daasolv.F` | DAA solver for acoustic FSI |
| `daasolvp.F` | DAA solver with porosity |
| `bemsolv.F` | Boundary Element Method solver (acoustic) |
| `bemsolvp.F` | BEM solver with porous boundary |

## Acoustic Boundary Element Method (BEM)

The BEM formulation (`bemsolv.F`, `daasolv.F`) is used for underwater shock / acoustic FSI problems. The Doubly Asymptotic Approximation (DAA) couples the BEM acoustic pressure field with the structural dynamic response at the fluid-structure interface.

The DAA approximates the fluid radiation impedance on the wet surface:
- For long-wavelength (low-frequency) excitation: plane-wave approximation
- For short-wavelength (high-frequency): radiation-damping asymptote
- DAA blends both limits smoothly

## Incompressible Flow (`incpflow.F`)

The incompressible Navier-Stokes solver handles viscous flows where density variations are negligible. It uses a pressure-velocity coupling (projection method) to enforce the incompressibility constraint `∇·v = 0`.

## Relation to ALE

The compressible ALE/Euler solver in `ale/` uses an EOS-driven pressure and handles supersonic flows, shocks, and explosions. This `fluid/` subsystem addresses subsonic, incompressible, viscous flows. Both can coexist in one model via domain coupling at the interface.

## Related Documentation

- `engine/source/ale/README.md` — compressible ALE/Euler solver
- `engine/source/interfaces/README.md` — TYPE22 fluid-structure coupling interface
- `engine/source/boundary_conditions/README.md` — EBCS pressure/velocity BCs for fluid domains
