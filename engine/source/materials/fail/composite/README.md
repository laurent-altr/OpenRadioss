# Composite Failure Criterion (`engine/source/materials/fail/composite/`)

General composite failure framework combining multiple sub-criteria (fiber, matrix, delamination) for layered shell elements.

## Key Files

| File | Role |
|------|------|
| `fail_composite_c.F90` | Composite failure check for solid elements |
| `fail_composite_s.F90` | Composite failure check for shell/laminate elements |

## Description

This module provides a unified entry point for composite failure that can combine:
- Fiber tension/compression modes
- Matrix tension/compression modes  
- Interlaminar shear failure (delamination indicator)

The specific sub-criterion activated depends on the material law flags (e.g., LAW025 PLY STACK definition). After failure detection, ply-by-ply stiffness degradation factors are applied: a failed ply loses load-carrying capacity in the failed mode while intact plies continue to contribute. Suitable for progressive failure analysis of laminated CFRP and GFRP structures.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/hashin/README.md` — Hashin sub-criteria
- `engine/source/materials/fail/changchang/README.md` — Chang-Chang with degradation
- `starter/source/properties/composite_options/README.md` — laminate stack definition
