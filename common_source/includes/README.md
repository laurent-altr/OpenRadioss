# Common Include Files (`common_source/includes/`)

C header files and Fortran include files shared between starter and engine.

## Files

| File | Contents |
|------|---------|
| `checksum.h` | C header: checksum computation function declaration (used by both Fortran and C code) |
| `elements.inc` | Fortran include: element type code constants (`ISHELL`, `ISOLID`, `IBEAM`, etc.) |
| `inter18.inc` | Fortran include: TYPE18 (contact surface-to-surface) interface array layout constants |
| `inter22.inc` | Fortran include: TYPE22 interface array layout constants |
| `kincod_c.inc` | C-compatible include: kinematic constraint codes (shared with Fortran via ISO C binding) |
| `qaprint_c.inc` | C include: QA print format codes |
| `weights_p4linux964_spmd.inc` | Fortran include: Gauss quadrature weights for P4 (4-node) element on Linux x86-64 SPMD build |
| `weights_p4linux964_spmd_avx512.inc` | Same, AVX-512 variant |
| `weights_p4linux964_spmd_sse3.inc` | Same, SSE3 variant |
| `weights_p4linuxa964_spmd.inc` | ARM (AArch64) variant of quadrature weights |

## Quadrature Weight Files

The `weights_*.inc` files contain precomputed Gauss quadrature weights and abscissae for specific element topologies. Multiple variants exist for different SIMD instruction sets — the build system selects the appropriate file. Inlining weights as includes avoids runtime function look-up and enables auto-vectorisation.

## Related Documentation

- `common_source/modules/README.md` — Fortran 90 modules (preferred over `INCLUDE` for new code)
- `.github/copilot-instructions.md` — coding standards; prefer modules over legacy includes
