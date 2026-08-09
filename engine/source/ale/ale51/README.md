# ALE51 Scheme (`engine/source/ale/ale51/`)

Implements the ALE51 advection scheme — a flux-corrected transport (FCT) ALE method with anti-diffusion for sharp interface tracking.

## Key Files

| File | Role |
|------|------|
| `ale51_gradient_reconstruction.F` | Gradient reconstruction for ALE51 |
| `ale51_gradient_reconstruction2.F` | Alternative gradient reconstruction |
| `ale51_antidiff2.F` | 2D anti-diffusion correction |
| `ale51_antidiff3.F` | 3D anti-diffusion correction |
| `ale51_upwind2.F` | 2D upwind flux |
| `ale51_upwind3.F` | 3D upwind flux |
| `ale51_init.F` | Scheme initialisation |
| `ale51_apply_remote_flux.F90` | Apply fluxes from remote MPI cells |
| `ale51_finish_int22.F90` | Finalize TYPE22 interface handling |

## FCT / Anti-Diffusion

ALE51 uses Flux-Corrected Transport (FCT):
1. Compute a low-order (diffusive) flux — guaranteed monotone
2. Compute a high-order (anti-diffusive) correction
3. Apply as much anti-diffusion as possible without creating new extrema

This gives near-second-order accuracy in smooth regions while maintaining sharp, monotone interfaces — better than pure MUSCL for material interfaces with large density jumps.

## Related Documentation

- `engine/source/ale/alefvm/README.md` — FVM framework ALE51 runs within
- `engine/source/ale/README.md` — parent ALE directory
