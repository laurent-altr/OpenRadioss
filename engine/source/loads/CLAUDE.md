# engine/source/loads/

## Purpose
External loading: concentrated nodal forces, gravity, initial velocities, pressure
loads, centrifugal loads, laser heating, and pressure-blast loads. These routines
assemble external force contributions into `NODES%A` each cycle (Step 0 / Step 5
in the `RESOL` cycle).

## Sub-directories and files

| Path | Contents |
|------|----------|
| `general/force.F90` | Main concentrated force dispatcher (`FORCE`): reads `/CLOAD` and dispatches time-function-scaled forces to `NODES%A` |
| `general/force_imp.F` | Concentrated force for implicit solver |
| `general/forcefingeo.F` | Force in finite geometry (follower force) |
| `general/forcepinch.F` | Force on pinched nodes |
| `general/grav/gravit.F` | Gravity (`/GRAV`): adds `g * NODES%MS(I)` to `NODES%A(3,I)` |
| `general/grav/gravit_fvm_fem.F` | Gravity for FVM elements |
| `general/grav/gravit_imp.F` | Gravity for implicit |
| `general/inivel/inivel_init.F90` | Initial velocity initialization from `/INIVEL` |
| `general/inivel/inivel_start.F90` | Apply initial velocities at start of run |
| `general/inivel/inivel_dt2.F90` | Initial velocity at specified time |
| `general/load_pressure/load_pressure.F` | Pressure load on element surfaces (`/PLOAD`) |
| `general/load_centri/cfield.F` | Centrifugal load (`/CLOAD` centrifugal) |
| `general/load_centri/cfield_imp.F` | Centrifugal load for implicit |
| `general/load_pcyl/press_seg3.F` | Cylindrical pressure segment |
| `general/load_pcyl/pressure_cyl.F` | Cylindrical pressure load |
| `general/pfluid/` | Fluid pressure loads |
| `general/python_call_funct_cload.F90` | Python-scripted external force callback |
| `laser/laser1.F`, `laser2.F` | Laser heating load (surface absorbed energy) |
| `pblast/pblast.F` … `pblast_3.F` | Pressure-blast load: incident/reflected pressure from blast wave, Friedlander/ConWep models |

## Integration in the cycle
`FORCE` is called from `RESOL` at the beginning of each cycle (Step 0, line ~2960)
before element force loops. It accumulates external forces into `NODES%A`, so they
are available for the MPI exchange and `ACCELE` step.

## Dependencies
- Called by: `RESOL` (via `CALL FORCE(…)` and `CALL GRAVIT(…)`)
- Uses: `NODES%A`, `NODES%MS`, function tables (`FUNCT` arrays from `LECINP`)
