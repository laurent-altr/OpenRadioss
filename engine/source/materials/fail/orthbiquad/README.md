# Orthotropic Bi-Quadratic Failure Criterion (`engine/source/materials/fail/orthbiquad/`)

Extends the bi-quadratic criterion to orthotropic materials, using material axes for strain decomposition.

## Key Files

| File | Role |
|------|------|
| `fail_orthbiquad_c.F` | Orthotropic bi-quadratic for solid elements |
| `fail_orthbiquad_s.F` | Orthotropic bi-quadratic for shell elements |

## Criterion

Same quadratic form as the isotropic bi-quadratic but evaluated in material coordinates `(L, T, N)` (longitudinal, transverse, normal). Separate failure surfaces are defined for in-plane and out-of-plane strain components, allowing independent calibration in fiber and matrix directions. Used for UD composites when Hashin or Puck criteria are not available from test data.

## Related Documentation

- `engine/source/materials/fail/README.md` — parent fail directory
- `engine/source/materials/fail/biquad/README.md` — isotropic bi-quadratic
- `engine/source/materials/fail/orthstrain/README.md` — orthotropic max strain
