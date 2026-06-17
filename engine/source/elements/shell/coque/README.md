# QEPH Shell (`engine/source/elements/shell/coque/`)

Quadrilateral shell element using the QEPH (Quasi-Exact Projection of Hourglass) formulation — the default high-performance shell in OpenRadioss.

## Key Files

| File | Role |
|------|------|
| `ccoor3.F` | Coordinate update and co-rotational frame |
| `cdefo3.F` | Strain-displacement matrix (QEPH kinematics) |
| `cderi3.F` | Shape function derivatives |
| `cevec3.F` | Eigenvector update for hourglass modes |
| `cbilan.F` | Internal force accumulation |
| `cdt3.F` | Time step estimate for QEPH shell |
| `ccoef3.F` | Hourglass control coefficients |
| `ccurv3.F` | Curvature computation for bending modes |
| `c4eoff.F` | Shell offset computation |

## Formulation

QEPH uses a reduced 1-point quadrature with stabilisation via physical hourglass control. The hourglass stabilisation coefficients are physically motivated (membrane + bending modes treated separately), giving better accuracy than pure numerical hourglass control, especially for curved shells and bending-dominated problems. Compatible with all material laws.

## Related Documentation

- `engine/source/elements/shell/README.md` — parent shell directory
- `engine/source/elements/shell/coqueba/README.md` — Belytschko-Tsay alternative
