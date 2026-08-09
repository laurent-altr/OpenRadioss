# Element Sandbox — Include (`tools/mockup/element_sandbox/source/include/`)

Fortran include files shared across the element sandbox source files.
These mirror a subset of the includes found in `engine/source/`.

## Key Files

| File | Role |
|------|------|
| `my_real.inc` | Defines `WP` kind parameter for single/double precision selection |
| `implicit_f.inc` | `IMPLICIT NONE` declaration enforced in all files |
| `mvsiz_p.inc` | `MVSIZ` vectorisation length parameter |
| `constant.inc` | Physical constants (π, etc.) |
| `com01_c.inc` | Common block `COM01`: global model counters |
| `comlock.inc` | OpenMP lock declarations |
| `lockon.inc` / `lockoff.inc` | OpenMP critical-section macros |
| `vectorize.inc` | Compiler-hint pragmas/directives for auto-vectorisation |

## Related Documentation

- `tools/mockup/element_sandbox/source/README.md` — parent directory
