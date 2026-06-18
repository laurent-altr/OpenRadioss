# Euler/ALE Boundary Conditions (`starter/source/boundary_conditions/ebcs/`)

Reads and initialises ALE/Euler boundary conditions: inlet/outlet, pressure, velocity, NRF (Non-Reflecting Far-field), cyclic, and propellant conditions.

## Key Files

| File | Role |
|------|------|
| `read_ebcs.F` | Main EBCS reader: dispatch by type keyword |
| `iniebcs.F` | Initialise EBCS data structures |
| `iniebcsp.F` | Initialise EBCS pressure state |
| `iniebcsp0.F` | Initialise reference pressure for EBCS |
| `iniebcs_dp.F` | Initialise pressure difference EBCS |
| `iniebcs_nrf_tcar.F` | Initialise NRF characteristic variables |
| `iniebcs_propellant.F90` | Initialise propellant inlet condition |
| `split_ebcs.F` | Split EBCS data across SPMD domains |
| `findele.F` | Find ALE elements adjacent to EBCS surface |
| `hm_read_ebcs_inlet.F` | Parse /EBCS/INLET: inflow velocity, density, energy |
| `hm_read_ebcs_vel.F` | Parse /EBCS/VEL: prescribed velocity on ALE boundary |
| `hm_read_ebcs_pres.F` | Parse /EBCS/PRES: prescribed pressure outlet |
| `hm_read_ebcs_normv.F` | Parse /EBCS/NORMV: normal velocity condition |
| `hm_read_ebcs_fluxout.F` | Parse /EBCS/FLUXOUT: flux outlet condition |
| `hm_read_ebcs_gradp0.F` | Parse /EBCS/GRADP0: zero-gradient pressure |
| `hm_read_ebcs_inip.F` | Parse /EBCS/INIP: initial pressure condition |
| `hm_read_ebcs_iniv.F` | Parse /EBCS/INIV: initial velocity condition |
| `hm_read_ebcs_monvol.F` | Parse /EBCS/MONVOL: control-volume airbag coupling |
| `hm_read_ebcs_nrf.F` | Parse /EBCS/NRF: non-reflecting far-field condition |
| `hm_read_ebcs_cyclic.F90` | Parse /EBCS/CYCLIC: cyclic (periodic) boundary |
| `hm_read_ebcs_valvin.F` | Parse /EBCS/VALVIN: valve-inlet condition |
| `hm_read_ebcs_valvout.F` | Parse /EBCS/VALVOUT: valve-outlet condition |
| `hm_read_ebcs_propellant.F90` | Parse /EBCS/PROPELLANT: solid propellant inflow |
| `ebcs_cyclic_surface_matching.F90` | Match cyclic boundary node pairs |
| `ebcs_cyclic_surface_matching_2d.F90` | 2D cyclic surface matching |
| `ebcs_cyclic_surface_matching_3d.F90` | 3D cyclic surface matching |

## Description

EBCS (Euler Boundary Conditions) apply state or flux constraints on the ALE/Euler domain boundaries. `read_ebcs.F` parses all `/EBCS` blocks; `split_ebcs.F` distributes the boundary faces to MPI domains using the mesh partition. Cyclic (periodic) boundaries require geometric matching of inlet and outlet surfaces, handled by `ebcs_cyclic_surface_matching.F90`.

## Related Documentation

- `starter/source/boundary_conditions/README.md` — parent directory
- `engine/source/boundary_conditions/README.md` — engine boundary condition enforcement
- `engine/source/ale/README.md` — ALE engine that consumes EBCS data
