# ALE Finite-Volume Method (`engine/source/ale/alefvm/`)

Finite-Volume Method (FVM) advection for the ALE solver — an alternative higher-order advection scheme.

## Key Files

| File | Role |
|------|------|
| `alefvm_init.F` | Initialise FVM data structures |
| `alefvm_accele.F` | FVM acceleration (body force) application |
| `alefvm_aflux3.F` | FVM ALE 3D mass flux |
| `alefvm_eflux3.F` | FVM Euler 3D mass flux |
| `alefvm_epsdot.F` | Strain rate computation for FVM elements |
| `alefvm_expand_mom2.F` | Momentum expansion in 2D FVM |
| `alefvm_freset.F` | Reset FVM flux accumulators |
| `alefvm_grav_init.F` | Gravity initialisation for FVM |
| `alefvm_gravity.F` | Apply gravity body force to FVM elements |
| `alefvm_gravity_int22.F` | Gravity for FVM with TYPE22 coupling |
| `afluxt.F` | ALE FVM thermal flux |
| `afluxt_int22.F90` | Thermal flux with TYPE22 coupling |
| `ale51_antidiff2.F` / `ale51_antidiff3.F` | ALE51 anti-diffusion (flux correction for higher order) |
| `ale51_apply_remote_flux.F90` | Apply flux from remote (MPI ghost) cells |
| `ale51_finish_int22.F90` | Finish ALE51 TYPE22 interface computation |
| `ale51_init.F` | ALE51 scheme initialisation |
| `ale51_upwind2.F` / `ale51_upwind3.F` | ALE51 upwind scheme |

## FVM vs MUSCL

The FVM scheme here is the finite-volume discretisation of the ALE conservation equations. The MUSCL scheme (in `alemuscl/`) provides the gradient reconstruction and limiting that makes the FVM second-order accurate.

The `ale51_*` files implement the ALE51 method — a specific FVM+MUSCL combination with flux correction (FCT/anti-diffusion) for minimal numerical diffusion at interfaces.

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/ale/alemuscl/README.md` — MUSCL gradient reconstruction
