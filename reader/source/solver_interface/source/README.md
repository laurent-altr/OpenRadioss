# Solver Interface Source (`reader/source/solver_interface/source/`)

C++ bridge between the SDI in-memory model and the Fortran starter kernel.
Exports `extern "C"` (`CDECL`) wrappers so Fortran routines can traverse the
SDI model without linking against C++ directly.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `cfg_reading/` | Model open/close, option iteration, attribute get/set |
| `elements/` | Per-element-type connectivity readers (`cpp_*_read`) |
| `includes/` | Shared headers: `GlobalModelSDI.h`, `buildmapping.h` |
| `messages/` | Fortran-callable message retrieval wrappers |
| `misc/` | LS-DYNA→Radioss converter entry point, parameter printing, env-var setup |
| `model/` | Top-level model build (`cfgreader`) and teardown |
| `nodes/` | Node coordinate and count readers |
| `submodels/` | Submodel (include file) hierarchy builder |

## Design

Every exported function follows the Fortran name-mangling convention —
three signatures are provided: `name_` (gfortran), `NAME` (MSVC), `name__`
(some compilers) — and a plain `name` alias.  The global singleton
`g_pModelViewSDI` (`GlobalModelSDI.h`) holds the active `ModelViewEdit`
pointer shared across all wrappers.

## Related Documentation

- `reader/source/solver_interface/README.md` — parent directory
- `reader/source/sdi/interface/README.md` — SDI model API
- `reader/source/sdi/converter/README.md` — conversion rule engine
