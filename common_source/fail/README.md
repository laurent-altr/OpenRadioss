# Shared Failure Model (common_source/fail)

This directory contains the single shared failure model that is used by both the **starter** (for initialisation) and the **engine** (for evaluation): the Newman-Raju stress intensity factor for fracture mechanics.

## File

| File | Role |
|------|------|
| `newman_raju.F90` | Newman-Raju stress intensity factor formula for surface cracks |

## Newman-Raju Formula

The Newman-Raju solution provides stress intensity factors (KI, KII, KIII) for semi-elliptical surface cracks in finite plates under tension or bending. It is an empirically calibrated analytical formula widely used in fatigue and fracture calculations.

Input parameters:
- Crack depth `a` and half-length `c` (semi-ellipse dimensions)
- Plate thickness `t` and half-width `b`
- Applied tension `S_t` and bending `S_b`
- Parametric angle `φ` along the crack front

Output: normalised stress intensity factor `F(a/c, a/t, c/b, φ)`.

## Scope

This routine is placed in `common_source/` because it is called during:
1. **Starter**: to initialise fatigue life tracking data structures
2. **Engine**: to evaluate crack growth at each time step

The bulk of failure criterion implementations (40+ models: Johnson-Cook, Hashin, Puck, Lemaitre, …) reside in `engine/source/materials/fail/` — those are engine-only.

## Related Documentation

- `engine/source/materials/README.md` — engine-side failure criteria (materials/fail/)
- `common_source/README.md` — overview of all shared common_source utilities
