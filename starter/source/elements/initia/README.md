# Element Initial State (`starter/source/elements/initia/`)

Reads and applies initial conditions at the element level — initial stress, strain, and material history states.

## Key Files

| File | Role |
|------|------|
| `initia.F` | Top-level driver: dispatch initial state to each element type |
| `lec_inistate.F` | Read `/INISTATE` keyword — initial element stress/strain state |
| `lec_inistate_d00_brick-check.F` | Validate initial state data for brick elements from DYNAIN |
| `lec_inistate_tri.F` | Read initial state for triangular shell elements |
| `lecfill.F` | Fill element buffer with the read initial state |
| `scaleini.F` | Scale initial stresses by a user factor (`/INISTATE, SCALE`) |
| `inirig_mat.F` | Initialise material state for rigid elements (zero-stress case) |
| `inivoid.F` | Set initial void (fully deleted) state for elements with active void flags |
| `ini_fvminivel.F` | Initialise finite-volume minimum velocity for ALE elements |
| `hm_read_inistate_d00.F` | Read initial state from HM binary in d00 format |
| `hm_yctrl.F` | Y-direction control for HM initial state mapping |
| `spmd_msin.F` | MPI scatter of initial state data to all ranks |
| `spmd_msin_addmass.F` | MPI scatter of added-mass initial conditions |

## Initial State Sources

Initial element stress/strain can come from:
1. `/INISTATE` keyword in the input deck — user-specified stress tensor per element
2. DYNAIN file from a previous simulation (forming-to-crash chaining)
3. HM binary (`hm_read_inistate_d00.F`)

## Related Documentation

- `starter/source/initial_conditions/README.md` — nodal initial conditions (`/INIVEL`, `/INISTA`)
- `starter/source/elements/README.md` — parent elements directory
