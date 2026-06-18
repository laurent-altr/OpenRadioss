# Solver Interface — Includes (`reader/source/solver_interface/source/includes/`)

Shared headers included by all solver-interface translation units.

## Key Files

| File | Role |
|------|------|
| `GlobalModelSDI.h` | Declares `g_pModelViewSDI` global pointer and the `GlobalModelSDI*` accessor / mutator functions used by every wrapper |
| `buildmapping.h` | `BuildMapping` entry point: builds the LS-DYNA → Radioss entity-ID mapping table using `dyna2rad` converter and Boost `unordered_map` |
| `dll_settings.h` | `CDECL` macro and DLL export settings for Windows |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/sdi/interface/README.md` — `ModelViewEdit` type
