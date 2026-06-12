# includes/

## Purpose
Global Fortran `.inc` include files and C headers shared across starter and engine. These are legacy shared-state mechanisms (COMMON blocks, parameter constants, header definitions). New code should use Fortran modules instead.

## Files

### Element and kinematic definitions
| File | Description |
|------|-------------|
| `elements.inc` | Integer parameters for element node counts: `NIXS`=11 (solid), `NIXC`/`NIXQ`=7 (shell), `NIXT`=5 (triangle), `NIXP`=6 (pentahedron), `NIXR`=6 (rigid) |
| `kincod_c.inc` | COMMON block arrays for kinematic boundary condition codes (IBC, ITF, IWL, IRB, IRB2, IVF, IRV, IJO, IRBM, ILMULT, IRLK, IKRBE2, IKRBE3); 0:8191 elements each |

### Interface COMMON blocks
| File | Description |
|------|-------------|
| `inter18.inc` | COMMON blocks `INTER18_i`, `INTER18_l`, `INTER18_r` for interface type 18 (truss); includes `INTER18_AUTOPARAM`, density/velocity/surface arrays, variable-gap flag |
| `inter22.inc` | COMMON blocks `INTER22_i`, `INTER22_r` for interface type 22 (brick/surface intersection); parameters `NSUB22`, `IDT_INT22`, law flags `I22LAW37`/`I22LAW51`/`I22_ALEUL` |

### QA output keywords
| File | Description |
|------|-------------|
| `qaprint_c.inc` | QA print keyword list: `NQAKEYLIST_AVAIL`=127 keyword strings (e.g., `'PROPERTIES'`, `'MATERIALS'`, `'INTERFACES'`) for selective diagnostic output |

### C/C++ headers
| File | Description |
|------|-------------|
| `checksum.h` | C++ header for MD5 checksum computation; defines `_FCALL`, `BUFFERSIZE`=4096 |

### Load-balancing weight tables
One include file per CPU microarchitecture; each defines `TPSREF` (reference CPU time per element) and `SOL1TNL` (per-element-type cost array) for dynamic load balancing.

| File | Architecture | TPSREF |
|------|-------------|--------|
| `weights_p4linux964_spmd.inc` | Broadwell / AVX2 | 4.784×10⁻⁷ |
| `weights_p4linux964_spmd_sse3.inc` | Sandy Bridge / SSE3 | 7.658×10⁻⁷ |
| `weights_p4linux964_spmd_avx512.inc` | Skylake / AVX-512 | 2.492×10⁻⁷ |
| `weights_p4linuxa964_spmd.inc` | ARM Cavium ThunderX2 | 8.130×10⁻⁷ |

## Notes
- COMMON blocks in `.inc` files are a legacy pattern; prefer modules for new code.
- The active `weights_*.inc` file is selected at build time via preprocessor macros.
