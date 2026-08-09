# MUSCL Advection (`engine/source/ale/alemuscl/`)

Monotone Upstream-centred Scheme for Conservation Laws (MUSCL) advection for ALE simulations.

## Key Files

| File | Role |
|------|------|
| `gradient_reconstruction.F90` | Reconstruct gradient of a field variable within each cell (Green-Gauss or least-squares) |
| `gradient_reconstruction2.F` | Alternative gradient reconstruction |
| `gradient_limitation.F` | Apply slope limiter to prevent oscillations at discontinuities |
| `gradient_limitation2.F` | Second limiter variant |
| `alemuscl_upwind.F` | MUSCL upwind flux: extrapolate to face, compute interface flux |
| `alemuscl_upwind2.F` | Upwind variant 2 (for multi-fluid) |
| `alemuscl_deallocate.F` | Free MUSCL working arrays after advection step |
| `conjugate_gradient.F` | Conjugate gradient solver for implicit MUSCL |
| `conjugate_gradient_vec.F90` | Vectorised CG solver |
| `direct_solve.F` | Direct solver for small implicit systems |
| `geom.F` | Geometric quantities for MUSCL: face areas, cell volumes |
| `geom_vec.F90` | Vectorised geometry computation |
| `ale51_gradient_reconstruction.F` | Gradient reconstruction for ALE51 scheme |
| `ale51_gradient_reconstruction2.F` | ALE51 gradient reconstruction variant |

## MUSCL Scheme

MUSCL extends the first-order Godunov scheme to second-order accuracy:
1. **Reconstruct**: compute gradient of each variable in each cell
2. **Limit**: apply slope limiter (minmod, van Albada) to preserve monotonicity
3. **Extrapolate**: estimate values at cell faces from cell centre + limited gradient
4. **Upwind flux**: use Riemann solver at each face to compute net flux
5. **Update**: cell values updated with flux divergence

This gives 2nd-order accuracy in smooth regions while remaining non-oscillatory at shocks and interfaces.

## Related Documentation

- `engine/source/ale/README.md` — parent ALE directory
- `engine/source/ale/ale2d/README.md` — 2D ALE uses MUSCL
- `engine/source/ale/ale3d/README.md` — 3D ALE uses MUSCL
