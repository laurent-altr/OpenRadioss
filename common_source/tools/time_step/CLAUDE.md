# tools/time_step/

## Purpose
Timestep computation utility: computes the target timestep based on the mass-stiffness coupling criterion and an absorbed energy percentage threshold. Used in the add-mass (mass scaling) feature.

## Files

| File | Description |
|------|-------------|
| `find_dt_target.F` | Subroutine `find_dt_target` — computes the target timestep from the mass-stiffness coupling ratio and the user-specified absorbed energy percentage threshold; called from the add-mass statistics routine |

## Dependencies
- Uses: `modules/precision_mod.F90`
- Used by: engine mass-scaling (`add_mass_stat`) routines
