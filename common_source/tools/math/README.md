# Mathematical Utilities (`common_source/tools/math/`)

Portable mathematical utilities for floating-point operations.

## Key Files

| File | Role |
|------|------|
| `precision.c` | C-level precision utilities: `FLT_EPSILON`, `DBL_EPSILON`, machine epsilon detection at runtime |

## Purpose

This directory provides machine-precision constants and portable floating-point utilities that cannot rely on Fortran compiler-specific intrinsics across all supported platforms (Linux x86-64, ARM, etc.).

The `precision.c` file exposes C `float.h` constants to Fortran via a C-to-Fortran linkage, ensuring the same values are used regardless of the Fortran compiler's `EPSILON()` implementation.

## Usage

For working-precision constants, prefer `PRECISION_MOD` (defined in `common_source/modules/`), which provides `WP` (working precision kind) and the corresponding epsilon. Use this `math/` directory only for C-interop cases where `PRECISION_MOD` is not available.

## Related Documentation

- `common_source/tools/README.md` — parent tools directory
- `common_source/modules/README.md` — `PRECISION_MOD` (preferred precision constants)
