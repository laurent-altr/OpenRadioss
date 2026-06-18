# Solver Interface — Includes (`reader/source/solver_interface/source/includes/`)

Shared headers used across all `solver_interface/source/` subdirectories.

## Key Files

| File | Role |
|------|------|
| `GlobalModelSDI.h` | Declares global `g_pModelViewSDI` singleton, `Get_ModelViewSDI()` accessor, and all `GlobalModelSDI*` Fortran-callable C functions |
| `buildmapping.h` | Declares `BuildMapping()` — drives the LS-DYNA→Radioss rule-based conversion using `sdiConvert::Convert` |
| `dll_settings.h` | `CDECL` and DLL-export macros for cross-platform symbol visibility |

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/sdi/interface/README.md` — `ModelViewEdit` type used by `g_pModelViewSDI`
