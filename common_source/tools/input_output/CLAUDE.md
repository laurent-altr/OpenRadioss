# tools/input_output/

## Purpose
Binary database read/write for restart files and low-level C I/O primitives. `write_routines.c` is the C backend called by `comm/write_mod.F`; `write_db.F` and `read_db.F` are the primary restart I/O routines used throughout the solver.

## Files

| File | Description |
|------|-------------|
| `write_routines.c` | Low-level C: file open/close/seek, IEEE ASCII format conversion, write of single values and arrays (int/float/double); called by `comm/write_mod.F` |
| `write_db.F` | Subroutine `WRITE_DB` — binary restart database writer; called extensively to write all simulation data (elements, nodes, materials, etc.) |
| `read_db.F` | Subroutine `READ_DB` — binary restart database reader; mirrors `write_db` for simulation state recovery |

## Notes
- `write_db` / `read_db` are very widely called — any new persistent data type added to the solver typically requires corresponding read/write calls here.

## Dependencies
- `write_routines.c` is called from `comm/write_mod.F`
- Used by: restart I/O routines in engine and starter throughout both source trees
