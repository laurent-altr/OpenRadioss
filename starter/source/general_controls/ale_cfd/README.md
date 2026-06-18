# ALE/CFD Control Parameters (`starter/source/general_controls/ale_cfd/`)

Reads ALE and CFD global control cards that configure the advection scheme, solver type, and upwinding strategy.

## Key Files

| File | Role |
|------|------|
| `hm_read_ale_muscl.F` | Parse /ALE/MUSCL: MUSCL scheme order, limiter selection |
| `hm_read_ale_solver.F` | Parse /ALE/SOLVER: solver type (FCT, MUSCL, van Leer), flux split |
| `hm_read_upwind.F` | Parse /UPWIND: first-/second-order upwinding flag for CFD |

## Description

These readers populate the global ALE/CFD control parameters stored in `ale_param_mod`. The MUSCL card selects the spatial reconstruction order (1st or 2nd) and the slope limiter (minmod, van Leer, superbee). The SOLVER card selects the advection algorithm (FCT51, van Leer, donor-cell). The UPWIND card controls the upwind bias for Euler/Navier-Stokes advection terms.

## Related Documentation

- `starter/source/general_controls/README.md` — parent directory
- `starter/source/general_controls/ale_grid/README.md` — ALE grid motion control
- `engine/source/ale/README.md` — ALE/Euler engine implementation
