# CFGIO (`reader/source/cfgio/`)

Configuration file I/O layer: reads and writes HyperWorks CFG-format model files (the internal binary format used by HyperMesh/HyperCrash for model exchange with OpenRadioss).

## Subdirectory

| Directory | Contents |
|-----------|---------|
| `MODEL_IO/` | Core CFG model reader/writer: `hw_cfg_reader.cpp` (CFG parser), `hcio.h`/`hcioi_utils.cpp` (I/O utilities), `mec_component.cpp` (component handling), `io_types.h` (type definitions), HyperWorks solver-info interface (`hcioi_solverinf.h`) |

## Architecture

CFGIO provides the bridge between HyperWorks GUI tools (HyperMesh, HyperCrash) and the OpenRadioss reader library. It:
1. Parses binary CFG model files produced by HyperMesh
2. Populates the SDI (Solver Data Interface) model objects
3. Or writes SDI model data back to CFG format for round-trip model transfer

The `hw_cfg_reader.cpp` is the main entry point for reading, driven by the `cfgkernel` parsing infrastructure.

## Related Documentation

- `reader/source/README.md` — reader architecture overview
- `reader/source/cfgkernel/README.md` — CFG kernel (expression/keyword parser)
- `reader/source/sdi/README.md` — SDI model data interface
