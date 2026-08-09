# Material Tools (`starter/source/materials/tools/`)

Utility routines shared across multiple material law initialisations in the starter.

## Key Files

| File | Role |
|------|------|
| `eos_table_copy.F90` | Deep copy of EOS table data (for material duplication) |
| `eosfun_usr2sys.F90` | Convert user-defined EOS function IDs to system-internal IDs |
| `fail_fun2sys.F` | Convert user failure curve function IDs to system IDs |
| `fail_tab2sys.F` | Convert user failure table IDs to system IDs |
| `func2d_deintersect.F` | Remove overlapping regions from 2D function tables |
| `func_table_copy.F90` | Deep copy of function/table data |
| `mat_func_deintersect.F90` | De-intersect material function references |
| `mat_table_copy.F90` | Deep copy of material table data |
| `finter_1d.F90` | 1D piecewise-linear interpolation for use within material parameter preprocessing |
| `law69_nlsqf_auto.F` | Automatic nonlinear least-squares fitting for LAW69 parameters |

## ID Conversion

OpenRadioss uses both user-facing IDs (as defined in the input deck) and internal (system) IDs for functions, tables, and curves. The `*2sys.F` routines convert user IDs to the internal IDs that the engine's dispatch tables use. This conversion happens once at startup and the internal IDs are written to the restart file.

## Related Documentation

- `starter/source/materials/README.md` — parent materials directory
- `starter/source/tools/curve/README.md` — function/curve data read by `curve/`
