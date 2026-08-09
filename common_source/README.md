# OpenRadioss Common Source

This directory contains Fortran and C source that is **shared between the starter and the engine**. Both binaries compile these files independently; there is no shared library at link time.

## Subdirectories

| Directory | Purpose |
|-----------|---------|
| `comm/` | Low-level communication utilities (stack I/O, memory, environment variables) |
| `eos/` | Equation of State (EOS) models — all EOS types used by both starter and engine |
| `fail/` | Failure model shared between starter and engine (Newman-Raju fracture criterion) |
| `fxbody/` | Fixed-body (rigid wall) data structures shared between starter and engine |
| `includes/` | Fortran `INCLUDE` files and legacy common-block headers |
| `input/` | Generic input-parsing utilities (token reading, card formats) |
| `interf/` | Interface buffer structures (`INTBUF_DEF_MOD`) and shared contact utilities |
| `linearalgebra/` | Basic linear algebra types (`matrix_mod`, `vector_mod`) |
| `modules/` | Fundamental Fortran modules used everywhere: precision, constants, arrays, ALE, BCs, … |
| `output/` | Shared output routines (error messages, unit management) |
| `qa/` | Quality-assurance helpers used in automated testing |
| `sortie/` | Shared error and diagnostic output utilities |
| `tools/` | General-purpose utility routines (sorting, searching, string handling) |

## Key Modules (in `modules/`)

| Module file | Exported symbols |
|-------------|-----------------|
| `precision_mod.F` | `WP` (working precision kind parameter) |
| `constant_mod.F` | `PI`, physical constants |
| `array_mod.F` | Dynamic array helpers |
| `check_mod.F` | Assertion / bounds-check utilities |
| `cast_mod.F90` | Type-casting helpers |
| `connectivity.F90` | Node/element connectivity structures |

## Equation of State Models (in `eos/`)

See `eos/README.md` for the full list. Major models include: Gruneisen, JWL, Tillotson, ideal gas, NASG, Noble-Abel, Osborne, polynomial, tabulated, LSZK, Puff, Sesame.

## Interf Buffer (`interf/`)

`INTBUF_DEF_MOD` defines the `intbuf_struct` used to pass integer metadata between the engine's element and interface routines. It is the primary bookkeeping structure for element-local integer data.

## Coding Note

Files in `common_source/` must compile cleanly against both starter and engine build environments. Avoid introducing dependencies on engine-only or starter-only modules here.

## Related Documentation

- `engine/source/README.md` — engine architecture
- `starter/source/README.md` — starter architecture
- `.github/copilot-instructions.md` — Fortran coding standards
