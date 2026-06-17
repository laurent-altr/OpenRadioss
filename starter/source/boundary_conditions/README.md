# Starter Boundary Conditions Subsystem

This subsystem reads and initialises all boundary condition definitions (BCS and EBCS keywords) for the restart file.

## Directory Structure

```
boundary_conditions/
├── ebcs/                       — Extended BC readers
├── hm_read_bcs_nrf.F90         — HM-format NRF BC reader
├── hm_read_bcs_wall.F90        — HM-format wall BC reader
├── init_bcs_nrf.F90            — NRF BC initialisation
└── init_bcs_wall.F90           — Wall BC initialisation
```

## Extended Boundary Conditions (`ebcs/`)

The EBCS family has complex geometry-matching and surface-pairing logic that is handled entirely in the starter. Key files:

| File | EBCS type | Description |
|------|----------|-------------|
| `ebcs_cyclic_surface_matching.F90` | `/EBCS/CYCLIC` | Find matching node pairs for cyclic symmetry (both 2D and 3D) |
| `ebcs_cyclic_surface_matching_2d.F90` | `/EBCS/CYCLIC` | 2D cyclic surface pair finding |
| `ebcs_cyclic_surface_matching_3d.F90` | `/EBCS/CYCLIC` | 3D cyclic surface pair finding |
| `findele.F` | — | Find elements adjacent to an EBCS surface |
| `hm_read_ebcs_cyclic.F90` | `/EBCS/CYCLIC` | HM-format cyclic BC reader |
| `hm_read_ebcs_fluxout.F` | `/EBCS/FLUXOUT` | Flux output BC reader |
| `hm_read_ebcs_gradp0.F` | — | Zero-gradient pressure BC reader |
| `hm_read_ebcs_inip.F` | `/EBCS/INIP` | Initial pressure BC reader |

## Cyclic Symmetry Setup (most complex EBCS)

`/EBCS/CYCLIC` requires pairing nodes on two symmetry surfaces so the engine can enforce the periodic constraint. The starter:
1. Identifies nodes on the master and slave cyclic surfaces
2. Finds the nearest matching node on the opposite surface for each node
3. Stores the pairing in the restart file

The 2D and 3D matchers use different geometric projections (2D: angle-based, 3D: rotation matrix).

## NRF and Wall BCs

`init_bcs_nrf.F90` and `init_bcs_wall.F90` set up the absorbing (non-reflecting) and rigid wall BCs. The HyperMesh readers provide an alternative input path for the same data.

## Related Documentation

- `engine/source/boundary_conditions/README.md` — runtime BC enforcement
- `starter/source/constraints/README.md` — kinematic constraints (BCS also here)
