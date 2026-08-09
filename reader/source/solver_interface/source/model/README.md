# Solver Interface — Model (`reader/source/solver_interface/source/model/`)

Top-level model construction and teardown: opens the input file, drives the
CFG/SDI reader, and returns control to the Fortran starter.

## Key Files

| File | Role |
|------|------|
| `cpp_build_model.cpp` | `cfgreader` / `cfgreader_inc` — main Fortran entry points: open a Radioss or LS-DYNA model file, populate `g_pModelViewSDI`, report reader messages |
| `cpp_delete_model.cpp` | Release the SDI model and free `g_pModelViewSDI` |

## Flow

```
cfgreader(filename)
  └─ RadiossblkReadModel / dynamainReadModel   ← format-specific readers
       └─ g_pModelViewSDI populated
  └─ (LS-DYNA only) BuildMapping               ← apply conversion rules
  └─ reader messages printed / returned
```

## Related Documentation

- `reader/source/solver_interface/source/README.md` — parent directory
- `reader/source/io/model_readers/radioss/README.md` — Radioss deck reader
- `reader/source/io/model_readers/ls-dyna/README.md` — LS-DYNA deck reader
