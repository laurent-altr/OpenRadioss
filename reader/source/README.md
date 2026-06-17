# OpenRadioss Open Reader — Source Architecture

The Open Reader is an optional third binary (separate from the starter and engine) that provides:
- A **CFG-format** input framework used by the starter and reader kernels
- A **LS-DYNA → OpenRadioss converter** (`dyna2rad`)
- An **SDI format** reader for Altair Simulation Data Interface
- A **solver interface** layer for third-party integration

## Subdirectories

| Directory | Purpose |
|-----------|---------|
| `cfgio/` | CFG-format file I/O management |
| `cfgkernel/` | CFG parser kernel — tokenisation, schema validation, data model |
| `dyna2rad/` | LS-DYNA keyword to OpenRadioss keyword converter |
| `io/` | Generic file I/O utilities used by the reader |
| `sdi/` | Altair SDI (Simulation Data Interface) format reader |
| `solver_interface/` | Interface layer for coupling the reader to external solvers |

## CFG Kernel (`cfgkernel/`)

The CFG kernel is the core of the Open Reader. It provides:

| Sub-component | Role |
|---------------|------|
| `KERNEL/` | Core parsing engine — reads CFG-format decks keyword by keyword |
| `KERNEL_BASE/` | Base types and error handling used by the kernel |
| `PARSER/` | Tokeniser and grammar rules for CFG format |
| `HCDI/` | Altair HCDI (HyperMesh Common Data Interface) compatibility layer |
| `GENERAL/` | General utilities shared across the kernel |
| `MESSAGE/` | User-visible message and error reporting |
| `MUNITS/` | Unit conversion support |
| `UTILS/` | Internal utility helpers |

## LS-DYNA Converter (`dyna2rad/`)

Translates LS-DYNA keyword files (`.k`, `.key`) to the OpenRadioss keyword format. Maps LS-DYNA `*KEYWORD` blocks to the corresponding `/KEYWORD` OpenRadioss equivalents. The converter is a standalone tool invoked separately from the starter/engine.

## SDI Reader (`sdi/`)

Reads result and model files in the Altair SDI binary format. Used for post-processing integration.

## Solver Interface (`solver_interface/`)

Provides a generic C/Fortran API that third-party tools can use to drive the reader subsystem programmatically (e.g. for GUI integration or batch conversion pipelines).

## Build

The Open Reader is built separately with the `-reader` flag:

```bash
./build_script.sh -arch=linux64_gf -mpi=ompi -reader
```

See `HOWTO.md` section "Build OpenRadioss with Open_Reader" for full instructions.

## Related Documentation

- Root `HOWTO.md` — build instructions including Open Reader
- `engine/source/README.md` — engine architecture
- `starter/source/README.md` — starter architecture
