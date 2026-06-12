# tools/math/

## Purpose
Portable low-level C math utilities: bitwise shift/logic operations and floating-point minimum value — callable from Fortran where intrinsic bit operations may not be available in legacy code.

## Files

| File | Description |
|------|-------------|
| `precision.c` | Functions `MY_SHIFTL`, `MY_SHIFTR`, `MY_AND`, `MY_OR` (portable bitwise operations) and `floatmin` (minimum representable float value); multiple Fortran calling-convention variants |

## Notes
- These routines exist for portability across Fortran compilers that historically lacked standard bit-manipulation intrinsics. Modern `.F90` code should prefer `ISHFT`, `IAND`, `IOR` instead.

## Dependencies
- Used by: legacy Fortran routines requiring bitwise operations
