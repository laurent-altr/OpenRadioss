# CFG I/O Model I/O (`reader/source/cfgio/MODEL_IO/`)

Model I/O layer for the CFG binary reader: factory readers, solver information interface, and utility functions for building the in-memory model from a CFG binary file.

## Key Files

| File | Role |
|------|------|
| `hw_cfg_reader.cpp` / `.h` | Main CFG binary file reader entry point |
| `hcio.h` | High-level CFG I/O API header |
| `hcioi_solverinf.h` | Solver information interface (version, model metadata) |
| `hcioi_utils.cpp` / `.h` | Utility functions for CFG I/O (string handling, ID lookup) |
| `io_types.h` | Type definitions for CFG I/O layer |
| `mec_component.cpp` | Mechanical component (part) assembly from CFG data |
| `cdr_reserveattribs.h` | Reserved attribute name declarations |
| `cfgio_model_factory_reader_po_exprtk.h` | Factory reader with expression toolkit (exprtk) for formula evaluation |

## Related Documentation

- `reader/source/cfgio/README.md` — parent directory
- `reader/source/cfgkernel/HCDI/README.md` — HCDI API that sits above this layer
