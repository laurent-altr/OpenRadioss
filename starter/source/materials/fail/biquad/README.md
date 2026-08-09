# Bi-Quadratic Failure (`starter/source/materials/fail/biquad/`)

Starter reader and initialisation for /FAIL/BIQUAD: bi-quadratic failure surface in principal strain space.

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_biquad.F` | Parse /FAIL/BIQUAD card parameters |
| `biquad_coefficients.F` | Compute bi-quadratic surface coefficients from input points |
| `biquad_tab.F90` | Build the tabulated failure surface from the bi-quadratic fit |
| `biquad_upd.F90` | Update failure indicator during initialisation |

## Description

FAIL/BIQUAD defines a failure locus as a bi-quadratic (elliptic) surface in (ε₁, ε₂, ε₃) space, fitted to user-supplied failure strain data points. `biquad_coefficients.F` solves the least-squares system to find the surface coefficients; `biquad_tab.F90` tabulates the locus for efficient engine lookup.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/biquad/README.md` — runtime failure evaluation
