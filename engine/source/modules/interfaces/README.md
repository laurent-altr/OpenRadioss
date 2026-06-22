# Engine Interface Modules (`engine/source/modules/interfaces/`)

Fortran module files defining interface (contact) data structures at the engine level.

## Key Files

| File | Role |
|------|------|
| `sh_offset_mod.F90` | Module for shell offset data in contact calculations |

## Description

This directory holds engine-specific interface module files that complement the shared modules in `common_source/modules/interfaces/`. `sh_offset_mod.F90` defines data structures for shell mid-surface offset corrections used in contact penetration calculations (TYPE7 and TYPE25 contact with shells have an offset between the contact surface and the mid-surface).

## Related Documentation

- `common_source/modules/interfaces/README.md` — shared interface modules (INTBUF, etc.)
- `engine/source/interfaces/README.md` — contact algorithm implementations
