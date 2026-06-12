# engine/source/boundary_conditions/

## Purpose
Boundary condition application during the time loop: fixed-body (rigid wall)
kinematics, general nodal BCs, Eulerian BCs, and thermal BCs. These set
constraints on nodal velocity, displacement, or temperature.

## Sub-directories

| Sub-dir | Contents |
|---------|----------|
| `fxbody/` | Fixed body (rigid wall): displacement/velocity enforcement for nodes attached to rigid bodies (`FXBDISPL`, `FXBODV`, `FXBODVP`, `FXBODFP`, `FXBSYS`, `FXBSGMAJ`, `FXBYFOR`, `FXBYVIT`, `FXBYPID`, `FXGRVCOR`) |
| `general/` | General nodal BCs: applied velocity (`IMPVEL` family), fixed BCs (`BCS`), rigid-body kinematic constraint update |
| `thermic/` | Thermal BCs: `CONVEC`, `FIXTEMP`, `FIXFLUX`, `TEMPUR` (temperature integration), `RADIATION`, plus their turn-on/off variants |
| `ebcs/` | Eulerian boundary conditions for ALE/Euler elements |

## Key routines

| Routine | Role |
|---------|------|
| `TEMPUR` | Integrates nodal temperature `NODES%TEMP` from heat flux; called inside `!$OMP PARALLEL` in `RESOL` after `ACCELE` |
| `FXBODV` | Applies fixed-body velocity to rigid-wall slave nodes |
| `CONVEC` | Adds convective thermal BC contribution to `NODES%A` / `NODES%MCP` |
| `FIXTEMP` | Applies prescribed temperature BC |
| `IMPVEL` | Applies imposed-velocity BC (from `/BCS` keyword) |

## Integration in the cycle
Boundary condition routines are called at multiple points in `RESOL`:
- Before element forces: kinematic constraints are initialized (`RESOL_INIT`)
- After `ACCELE`: `TEMPUR` updates temperature
- Inside the velocity/coordinate update: imposed velocities overwrite `NODES%V`

## Dependencies
- Called by: `RESOL`, `RESOL_INIT`
- Uses: `NODES` structure (`nodal_arrays_`), `SKEW_MOD` (for skewed BCs)
