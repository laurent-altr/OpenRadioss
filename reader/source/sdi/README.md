# SDI — Solver Data Interface (`reader/source/sdi/`)

Abstract C++ interface for accessing solver model data (nodes, elements, materials, boundary conditions) independent of file format or solver type.

## Subdirectories

| Directory | Contents |
|-----------|---------|
| `converter/` | Format converters: use SDI API to transform between formats |
| `interface/` | SDI C++ abstract interface headers: `sdiEntity.h`, `sdiElement.h`, `sdiDefs.h`, `sdiIdManager.h`, `sdiSelectionObserver.h`, `sdiTypeMapper.h`, `sdiMessageHandler.h` |
| `tools/` | SDI utilities: helper functions for SDI entity manipulation |
| `utils/` | Low-level SDI support utilities |

## Architecture

SDI is the data model layer used by HyperWorks reader and GUI tools. Key concepts:
- **sdiEntity**: base class for all model objects (nodes, elements, materials, properties)
- **sdiElement / sdiEntityBase**: element and base entity abstractions
- **sdiIdManager**: manages entity IDs across formats (handles ID conflicts, offsets)
- **sdiTypeMapper**: maps solver-specific type codes to generic SDI type enums
- **sdiMessageHandler**: unified warning/error reporting interface

The SDI abstraction allows the same viewer, converter, and analysis tools to work with Radioss, DYNA, Abaqus, and Nastran model data through a single API.

## Related Documentation

- `reader/source/io/README.md` — model reader layer above SDI
- `reader/source/cfgkernel/README.md` — keyword parser that feeds SDI
