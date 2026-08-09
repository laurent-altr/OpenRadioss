# Sahraei Failure (`starter/source/materials/fail/sahraei/`)

Starter reader for /FAIL/SAHRAEI: Sahraei battery cell failure criterion for lithium-ion cells under mechanical abuse (crush, indentation).

## Key Files

| File | Role |
|------|------|
| `hm_read_fail_sahraei.F` | Parse /FAIL/SAHRAEI card parameters: axial/lateral failure strains |

## Description

The Sahraei criterion evaluates `(ε₁/ε₁_max)² + (ε₂/ε₂_max)² ≥ 1` in principal strain space. The elliptical locus is fitted to experimental crush data for cylindrical and prismatic battery cells.

## Related Documentation

- `starter/source/materials/fail/README.md` — parent directory
- `engine/source/materials/fail/sahraei/README.md` — runtime failure evaluation
